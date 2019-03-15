package explanationregeneration

import data.question.{ExplanationRow, MCExplQuestion}
import edu.arizona.sista.struct.Counter
import explanationgraph.{LookupLemmatizer, TableKnowledgeCategories, TableRow, TableStore}

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._
import ExplanationRegeneration.{ROLE_CENTRAL, ROLE_GROUNDING, ROLE_LEXGLUE, ROLE_BACKGROUND, contentTags}
import ExplRowPool._

import scala.collection.mutable

/**
  * The gold explanation for a given question in WorldTree takes the form of a list of facts (tablestore rows).
  * Here, explanation regeneration is framed as a ranking task, where an algorithm must successfully rank the gold
  * explanation rows for a given question to the top of a list containing all possible facts (i.e. all the facts
  * present in the tablestore).
  *
  * This class makes a list of RowEvals, one for each possible tablestore row, where these RowEvals contain
  * sets of features that allow them to be compared/ranked, and the gold rows to (ideally) be ranked to the top of the
  * list.
  *
  * Created by peter on 3/16/18.
  */
class ExplRowPool(question:MCExplQuestion, answerCandidate:Int, tablestore:TableStore) {
  var rowEvals = new ArrayBuffer[RowEval]

  // Add a tablestore row to this pool, with a blank rowEval
  def addTablestoreRow(row:TableRow): Unit = {
    val rowEval = new RowEval(row)
    rowEvals.append(rowEval)
  }


  /*
   * Ranking
   */

  // Rank each row given it's score
  def rank(): Unit = {
    rowEvals = rowEvals.sortBy(- _.getScore())
  }

  // Add scores for each row evaluation generated externally -- e.g., by a ranking classifier
  def populateExternalScores(in:Array[Double]) {
    assert (in.length == rowEvals.length)
    for (i <- 0 until in.length) {
      rowEvals(i).setScore( in(i) )
    }
  }

  // Make parallel arrays of (label, feature) pairs, for the ranking classifier dataset generation
  def mkLabelFeaturePairs(relabelZeroFeaturePositives:Boolean = true):(Array[Int], Array[Counter[String]]) = {
    val numRowEvals = rowEvals.length
    val labels = Array.fill[Int](numRowEvals)(0)
    val features = new Array[Counter[String]](numRowEvals)

    // Get a list of UIDs that are correct for this question
    val correctUIDs = question.expl.map(_.uid)

    for (i <- 0 until numRowEvals) {
      // Label: 1 if a row in this explanation, 0 if not
      labels(i) = 0
      if (correctUIDs.contains(rowEvals(i).row.uid)) labels(i) = 1
      // Features: features stored for each row, generated elsewhere
      features(i) = rowEvals(i).features

      //## If the feature values are all zero, AND the label is positive (1), then the classifier likely has no chance of learning this, and it will only confuse it.
      //## If enabled, this feature resets these to zero.
      if ((relabelZeroFeaturePositives == true) && (labels(i) == 1)) {
        println ("RE: " +  rowEvals(i).features )
        breakable {
          for (key <- rowEvals(i).features.keySet) {
            if (rowEvals(i).features.getCount(key) != 0) break()
          }
          // If we reach here, the feature space is entirely zeros. Relabel this training example as a negative example.
          labels(i) = 0
          println ("* setting label to zero, as features are zero: " + rowEvals(i).features.toString())
        }
      }
    }

    // Return
    (labels, features)
  }


  /*
   * Scoring
   */

  // MAP, by category (grounding, central, lexical glue)
  // MAP, by category (rows with lexical overlap, rows without lexical overlap)
  // MAP, by category (rows from retrieval, inference supporting, or complex inference type tables)
  // MAP, overall

  def getScores():Counter[String] = {
    val scores = new Counter[String]

    // Average precision
    scores.setCount(SCORE_MAP, averagePrecision())

    // Average precision by role
    scores.setCount(SCORE_MAP_CENTRAL, averagePrecisionCentral())
    scores.setCount(SCORE_MAP_GROUNDING, averagePrecisionGrounding())
    scores.setCount(SCORE_MAP_LEXGLUE, averagePrecisionLexGlue())
    scores.setCount(SCORE_MAP_BACKGROUND, averagePrecisionBackground())

    // Average precision by lexical overlap
    val (mapLexOverlap, mapNoOverlap) = averagePrecisionByLexicalOverlap()
    scores.setCount(SCORE_MAP_LEXOVERLAP, mapLexOverlap)
    scores.setCount(SCORE_MAP_NOLEXOVERLAP, mapNoOverlap)


    // Average precision by table knowledge type (retrieval, inference supporting, complex inference)
    scores += averagePrecisionByTableCategory()

    // Precision@N
    scores.setCount(SCORE_PRECISIONAT1, precisionAtN(1))
    scores.setCount(SCORE_PRECISIONAT2, precisionAtN(2))
    scores.setCount(SCORE_PRECISIONAT3, precisionAtN(3))
    scores.setCount(SCORE_PRECISIONAT4, precisionAtN(4))
    scores.setCount(SCORE_PRECISIONAT5, precisionAtN(5))

    /*
    // Set single-count counter keys, so that the counter can be added and averaged easily later on
    val keySet = scores.keySet.toArray
    for (key <- keySet) {
      if (scores.getCount(key) > 0.0) {
        scores.setCount(key + "_NUM", 1.0)
      }
    }
    */

    println (scores.toString())
    // Return
    scores
  }



  /*
   * Scoring: Precision@N
   */
  def precisionAtN(n:Int):Double = {
    val ranks = ranksOfCorrectRowsHelper(question.expl.toArray, rowEvals.toArray)

    var numCorrect:Double = 0
    for (i <- 0 until n+1) {
      if (ranks.contains(i)) {
        numCorrect += 1
      }
    }

    var score:Double = 0.0
    // Case 1: The gold explanation has more than N rows
    if (question.expl.size > n) {
      score = (numCorrect / n.toDouble)
    } else {
      // Case 2: The gold explanation has less than N rows, so taking (numCorrect/n) would never be able to yield a
      // perfect score, even if all rows were present.
      score = (numCorrect / question.expl.size.toDouble)
    }

    return score
  }


  /*
   * Scoring: Average precision
   */

  def averagePrecision():Double = {
    val ranks = ranksOfCorrectRowsHelper(question.expl.toArray, rowEvals.toArray)
    val AP = calculateAPFromRanks(ranks)
    // Return
    AP
  }


  /*
   * Scoring: Average precision by role
   */

  def averagePrecisionByRole(role:String):Double = {
    // Step 2: Separate gold explanation rows into those with/without the query role
    val goldExplRowsInRole = new ArrayBuffer[ExplanationRow]()
    val filterUIDs = new ArrayBuffer[String]()

    for (i <- 0 until question.expl.length) {
      val explRow = question.expl(i)

      if (explRow.role == role) {
        goldExplRowsInRole.append(explRow)
      } else {
        filterUIDs.append(explRow.uid)
      }
    }


    // Step 3: Remove rows from the pool that are correct, but have a role other than the query role
    val rowEvalsNoOther = new ArrayBuffer[RowEval]

    for (i <- 0 until rowEvals.length) {
      val rowEval = rowEvals(i)
      val uid = rowEval.row.uid

      if (!filterUIDs.contains(uid)) {
        rowEvalsNoOther.append(rowEval)
      }
    }


    // Step 4: Compute average precision
    val ranksRole = ranksOfCorrectRowsHelper(goldExplRowsInRole.toArray, rowEvalsNoOther.toArray)
    val APRole = calculateAPFromRanks(ranksRole)

    //println ("Ranks of correct rows (ROLE: " + role + ") (AP = " + APRole.formatted("%3.4f") + ") : " + ranksRole.mkString(", "))

    // Step 5: Return
    APRole
  }

  def averagePrecisionCentral():Double = {
    averagePrecisionByRole(ROLE_CENTRAL)
  }

  def averagePrecisionGrounding():Double = {
    averagePrecisionByRole(ROLE_GROUNDING)
  }

  def averagePrecisionLexGlue():Double = {
    averagePrecisionByRole(ROLE_LEXGLUE)
  }

  def averagePrecisionBackground():Double = {
    averagePrecisionByRole(ROLE_BACKGROUND)
  }

  /*
   * Scoring: Average precision by row having lexical overlap with question
   */
  def averagePrecisionByLexicalOverlap(onlyContentTags:Boolean = true):(Double, Double) = {
    // Step 1: Find lemmas in question and a given answer candidate
    val qWords = mutable.Set[String]()

    // Edge case: Invalid answer candidate -- return 0 score
    if (answerCandidate < 0) return (0, 0)

    // Question
    for (sent <- question.question.annotation.sentences) {
      val words = sent.words
      val tags = sent.tags.get
      for (i <- 0 until words.size) {
        var tag = tags(i)
        if (tag.length > 2) tag = tag.slice(0, 2)
        if ((contentTags.contains(tag)) || (onlyContentTags == false)) {
          if (words(i).length > 0) {
            val word = words(i).toLowerCase
            val lemma = LookupLemmatizer.getLemma(word)
            qWords += word
            qWords += lemma
          }
        }
      }
    }

    // Answer
    for (sent <- question.question.choices(answerCandidate).annotation.sentences) {
      val words = sent.words
      val tags = sent.tags.get
      for (i <- 0 until words.size) {
        var tag = tags(i)
        if (tag.length > 2) tag = tag.slice(0, 2)
        if ((contentTags.contains(tag)) || (onlyContentTags == false)) {
          if (words(i).length > 0) {
            val word = words(i).toLowerCase
            val lemma = LookupLemmatizer.getLemma(word)
            qWords += word
            qWords += lemma
          }
        }
      }
    }


    //## Debug:
    //println ("Question and answer lemmas: " + qWords.mkString(", "))

    // Step 2: Separate gold explanation rows into those with/without lexical overlap with the question/answer candidate
    val goldExplRowsLexOverlap = new ArrayBuffer[ExplanationRow]()
    val goldExplRowsNoOverlap = new ArrayBuffer[ExplanationRow]()

    for (i <- 0 until question.expl.length) {
      val explRow = question.expl(i)
      val uid = explRow.uid
      val row = tablestore.getRowByUID(uid)
      val words = row.getRowWordsStr()

      var hasOverlap:Boolean = false
      breakable {
        for (word <- words) {
          val lemma = LookupLemmatizer.getLemma(word)
          if ((qWords.contains(word)) || (qWords.contains(lemma))) {
            hasOverlap = true
            break()
          }
        }
      }

      if (hasOverlap) {
        goldExplRowsLexOverlap.append(explRow)
      } else {
        goldExplRowsNoOverlap.append(explRow)
      }
    }


    // Step 3: Separate into rows with lexical overlap, and rows without lexical overlap
    val rowEvalsLexOverlap = new ArrayBuffer[RowEval]
    val rowEvalsNoOverlap = new ArrayBuffer[RowEval]

    for (i <- 0 until rowEvals.length) {
      val rowEval = rowEvals(i)
      val row = rowEval.row
      val words = row.getRowWordsStr()

      var hasOverlap:Boolean = false
      breakable {
        for (word <- words) {
          val lemma = LookupLemmatizer.getLemma(word)
          if ((qWords.contains(word)) || (qWords.contains(lemma))) {
            hasOverlap = true
            break()
          }
        }
      }

      if (hasOverlap) {
        rowEvalsLexOverlap.append(rowEval)
      } else {
        rowEvalsNoOverlap.append(rowEval)
      }
    }


    // Step 4: Compute average precision
    val ranksLexOverlap = ranksOfCorrectRowsHelper(goldExplRowsLexOverlap.toArray, rowEvalsLexOverlap.toArray)
    val APLexOverlap = calculateAPFromRanks(ranksLexOverlap)

    val ranksNoOverlap = ranksOfCorrectRowsHelper(goldExplRowsNoOverlap.toArray, rowEvalsNoOverlap.toArray)
    val APNoOverlap = calculateAPFromRanks(ranksNoOverlap)


    //println ("Ranks of correct rows (lexical overlap subset) (AP = " + APLexOverlap.formatted("%3.4f") + ") : " + ranksLexOverlap.mkString(", "))
    //println ("Ranks of correct rows (no lexical overlap subset) (AP = " + APNoOverlap.formatted("%3.4f") + ") : " + ranksNoOverlap.mkString(", "))

    // Step 5: Return
    (APLexOverlap, APNoOverlap)
  }


  /*
   * Scoring: Table category (retrieval, inference-supporting, or complex inference)
   */
  def averagePrecisionByTables(includeTables:Array[String]):Double = {
    // Step 1: Filter the gold explanation rows to only those found in the list of tables
    val goldExplRowsInTables = new ArrayBuffer[ExplanationRow]()

    for (i <- 0 until question.expl.length) {
      val explRow = question.expl(i)
      val uid = explRow.uid
      val row = tablestore.getRowByUID(uid)
      val words = row.getRowWordsStr()

      if (includeTables.contains(row.tableName)) {
        goldExplRowsInTables.append(explRow)
      }
    }


    // Step 2: Filter the list of rowEvals to only include those found in the list of tables
    val rowEvalsInTables = new ArrayBuffer[RowEval]

    for (i <- 0 until rowEvals.length) {
      val rowEval = rowEvals(i)
      val row = rowEval.row
      val words = row.getRowWordsStr()

      if (includeTables.contains(row.tableName)) {
        rowEvalsInTables.append(rowEval)
      }
    }


    // Step 3: Compute average precision
    //val APInTables = calculateAPHelper(rowEvalsInTables.toArray, goldExplRowsInTables.toArray)
    val ranks = ranksOfCorrectRowsHelper(goldExplRowsInTables.toArray, rowEvalsInTables.toArray)
    val APInTables = calculateAPFromRanks(ranks)

    //println ("Ranks of correct rows (Tables: " + includeTables.mkString(", ") + ") (AP = " + APInTables.formatted("%3.4f") + ") : \n   " + ranksOfCorrectRowsHelper(goldExplRowsInTables.toArray, rowEvalsInTables.toArray).mkString(", "))

    // Step 4: Return
    APInTables
  }

  def averagePrecisionByTableCategory():Counter[String] = {
    val out = new Counter[String]
    val categories = TableKnowledgeCategories.getCategories()

    for (category <- categories) {
      val tableNames = TableKnowledgeCategories.getTablesInCategory(category)
      val ap = averagePrecisionByTables( tableNames )
      out.setCount("MAP_TABKT_" + category, ap)
    }

    // Return
    out
  }



  /*
   * Scoring: Other
   */

  // Calculate average precision from a set of ranks
  def calculateAPFromRanks(ranksIn:Array[Int]):Double = {
    var sumPrecisions:Double = 0.0
    val numRanks = ranksIn.length

    // Case: Empty ranks list
    if (numRanks == 0) return 0.0

    // Case: Non-empty ranks list
    for (i <- 0 until numRanks) {
      val numCorrectAtRank = i + 1
      val precision:Double = numCorrectAtRank.toDouble / ranksIn(i).toDouble
      sumPrecisions += precision
    }

    // Return average precision
    sumPrecisions / numRanks.toDouble
  }

  def ranksOfCorrectRows():Array[Int] = {
    ranksOfCorrectRowsHelper(question.expl.toArray, rowEvals.toArray)
  }

  // ranks (starting from 1)
  def ranksOfCorrectRowsHelper(toFindIn:Array[ExplanationRow], rowEvalsIn:Array[RowEval]):Array[Int] = {
    val out = new ArrayBuffer[Int]
    out.insertAll(0, offsetsOfCorrectRowsHelper(toFindIn, rowEvalsIn))
    for (i <- 0 until out.length) {
      out(i) += 1
    }
    // Return
    out.toArray
  }

  // offsets (0 indexed)
  def offsetsOfCorrectRowsHelper(toFindIn:Array[ExplanationRow], rowEvalsIn:Array[RowEval]):Array[Int] = {
    val toFind = new ArrayBuffer[ExplanationRow]    // List of UIDs
    toFind.insertAll(0, toFindIn )             // Add list of UIDs for gold explanation from question
    val ranks = new ArrayBuffer[Int]

    for (i <- 0 until rowEvalsIn.length) {
      // Step 1: Determine if the rowEval at the current rank is part of the correct explanation, or not
      breakable {
        for (j <- 0 until toFind.length) {
          if (rowEvalsIn(i).row.uid == toFind(j).uid) {
            ranks.append(i)
            toFind.remove(j)
            break()
          }
        }
      }

      // Check for early stopping condition
      if (toFind.length == 0) {
        return ranks.toArray
      }
    }

    // Edge case: Otherwise
    if (toFind.length > 0) {
      val missingUIDs = new ArrayBuffer[String]
      for (row <- toFind) {
        missingUIDs.append(row.uid)
      }
      println ("* WARNING: averagePrecision: toFind length is greater than zero (" + toFind.length +"). This likely indicates that a UID was not found. Ranks for this set will be incorrect. (UIDs = " + missingUIDs.mkString(", ") + ")" )
      for (i <- 0 until toFind.length) {
        ranks.append(-1)
      }
    }
    // Return
    ranks.toArray
  }


  /*
   * String methods
   */
  def getEvalPoolString(maxDisplay:Int = -1):String = {
    val os = new StringBuilder

    // Bound checking on maxDisplay
    var numToDisplay:Int = rowEvals.length
    if (maxDisplay > 0) {
      if (maxDisplay < numToDisplay) {
        numToDisplay = maxDisplay
      }
    }

    for (i <- 0 until numToDisplay) {
      os.append(i + ": \t" + rowEvals(i).toString() + "\n")
    }

    // Return
    os.toString()
  }


  override def toString():String = {
    val os = new StringBuilder

    // Question
    os.append( question.toString() + "\n" )

    // Gold explanation
    os.append("Explanation (Table Rows):\n")
    for (i <- 0 until question.expl.size) {
      val uid = question.expl(i).uid
      os.append("\t" + uid + " \t" + question.expl(i).role + " \t" + tablestore.getRowByUID(uid).toStringText() + "\n")
    }
    os.append("Answer Candidate: " + answerCandidate + " (" + question.question.choices(answerCandidate).text + ")\n")
    os.append("\n")


    //TODO: Evaluations
    os.append("Evaluations: \n")
    val maxDisp:Int = 20
    for (i <- 0 until math.min(rowEvals.size, maxDisp)) {
      os.append (i + " \t" + rowEvals(i).toStringHighlight(question) + "\n")
    }

    os.append(" Ranks of gold rows: " + ranksOfCorrectRows().mkString(", ") + "\n")
    os.append(" Average Precision: " + averagePrecision() + "\n")
    os.append(" Average Precision (CENTRAL): " + averagePrecisionCentral() + "\n")
    os.append(" Average Precision (GROUNDING): " + averagePrecisionGrounding() + "\n")
    os.append(" Average Precision (LEXGLUE): " + averagePrecisionLexGlue() + "\n")
    val (mapLexOverlap, mapNoOverlap) = averagePrecisionByLexicalOverlap(onlyContentTags = true)
    os.append(" Average Precision (LEXOVERLAP): " + mapLexOverlap + "\n")
    os.append(" Average Precision (NOLEXOVERLAP): " + mapNoOverlap + "\n")
    os.append(" Average Precision by Table Category: " + averagePrecisionByTableCategory().toString() + "\n")

    for (n <- 1 to 5) {
      os.append(" Precision@" + n + ": " + precisionAtN(n).formatted("%3.3f") + "\n")
    }


    // Return
    os.toString()
  }
}


// Storage class for storing the features for a single row, as well as generating a single unified score for that row based on those feature values.
class RowEval(val row:TableRow) {
  val features = new Counter[String]
  private var score:Double = 0.0

  /*
   * Scoring
   */
  // Get this row's score
  def getScore():Double = {
    score
  }

  // Set this row's score (usually from an external classifier, based on this row's features)
  def setScore(newScore:Double): Unit = {
    score = newScore
  }

  /*
   * String methods
   */

  // Display the row, the features for that row, and highlight whether the row is part of the gold explanation (*)
  def toStringHighlight(question:MCExplQuestion):String = {
    val os = new StringBuffer()

    // Counter text
    val counterKeysSorted = features.keySet.toArray.sorted
    os.append("[")
    for (key <- counterKeysSorted) {
      os.append(key + ":" + features.getCount(key).formatted("%3.3f") + ", ")
    }
    os.append("]")
    os.append(" \t")

    // Highlight rows that are part of the gold explanation
    breakable {
      for (explRow <- question.expl) {
        if (row.uid == explRow.uid) {
          os.append ("* ")
          break()
        }
      }
    }

    // Row text
    os.append( row.toStringText() )


    // Return
    os.toString
  }


  // Display the row, and the features for that row
  override def toString():String = {
    val os = new StringBuffer()

    // Counter text
    val counterKeysSorted = features.keySet.toArray.sorted
    os.append("[")
    for (key <- counterKeysSorted) {
      os.append(key + ":" + features.getCount(key).formatted("%3.3f") + ", ")
    }
    os.append("]")
    os.append(" \t")

    // Row text
    os.append( row.toStringText() )


    // Return
    os.toString
  }

}



object ExplRowPool {
  // Score defines
  val SCORE_MAP                 = "MAP"
  val SCORE_MAP_CENTRAL         = "MAP_CENTRAL"
  val SCORE_MAP_GROUNDING       = "MAP_GROUNDING"
  val SCORE_MAP_LEXGLUE         = "MAP_LEXGLUE"
  val SCORE_MAP_BACKGROUND      = "MAP_BACKGROUND"
  val SCORE_MAP_LEXOVERLAP      = "MAP_LEXOVERLAP"
  val SCORE_MAP_NOLEXOVERLAP    = "MAP_NOLEXOVERLAP"
  val SCORE_MAP_TABKT_RET       = "MAP_TABKT_RET"
  val SCORE_MAP_TABKT_RETLEX    = "MAP_TABKT_RET/LEX"
  val SCORE_MAP_TABKT_INFSUPP   = "MAP_TABKT_INSUPP"
  val SCORE_MAP_TABKT_COMPLEX   = "MAP_TABKT_COMPLEX"

  val SCORE_PRECISIONAT1        = "PRECISION_AT_1"
  val SCORE_PRECISIONAT2        = "PRECISION_AT_2"
  val SCORE_PRECISIONAT3        = "PRECISION_AT_3"
  val SCORE_PRECISIONAT4        = "PRECISION_AT_4"
  val SCORE_PRECISIONAT5        = "PRECISION_AT_5"

}
