package explanationregeneration

import java.io.{File, PrintWriter}
import java.util.{Locale, Properties}

import data.question.{ExamQuestionParserDynamic, MCExplQuestion, MCQuestion}
import edu.arizona.sista.learning._
import edu.arizona.sista.struct.{Counter, Counters}
import edu.arizona.sista.utils.StringUtils
import explanationgraph.{TableRow, TableStore}
import explanationregeneration.ExplRowPool._
import org.slf4j.LoggerFactory

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.control.Breaks._

/**
  * Created by peter on 3/15/18.
  */

class ExplanationRegeneration {

}

object ExplanationRegeneration {
  val VERSION = "1.0"
  val PROGRAM_TITLE = "WorldTree: ExplanationRegeneration version " + VERSION
  val logger = LoggerFactory.getLogger(classOf[ExplanationRegeneration])

  // Word filtering by content tags (noun, verb, adjective, adverb, preposition)
  val contentTags = Array("NN", "VB", "JJ", "RB")

  val ROLE_CENTRAL    = "CENTRAL"
  val ROLE_GROUNDING  = "GROUNDING"
  val ROLE_LEXGLUE    = "LEXGLUE"
  val ROLE_BACKGROUND = "BACKGROUND"


  /*
   * Term/Document Frequency
   */

  // Document frequency from all rows in the tablestore
  def mkDocFreqRows(tablestore:TableStore):Counter[String] = {
    val freq = new Counter[String]

    for (table <- tablestore.tables) {
      for (row <- table.rows) {
        // Determine document (number of rows a given word occurs in) frequency

        val dataColIdxs = row.dataColumns
        val rowCounter = new Counter[String]

        for (idx <- dataColIdxs) {
          val cellStr = row.cells(idx).toLowerCase.trim().replaceAll("\\s+", " ")
          val cellWords = cellStr.split(" ")

          for (word <- cellWords) {
            if (word.length > 0) {
              rowCounter.setCount(word, 1.0)
            }
          }
        }

        // Add row counter to global counter
        freq += rowCounter
      }
    }

    // Return
    freq
  }

  // Document frequency from all questions in a list
  def mkDocFreqQuestions(questions:Array[MCExplQuestion]):Counter[String] = {
    val freq = new Counter[String]

    for (question <- questions) {
      // Determine document (number of rows a given word occurs in) frequency
      val rowCounter = new Counter[String]

      // Question
      for (sent <- question.question.annotation.sentences) {
        for (word <- sent.words) {
          if (word.length > 0) {
            rowCounter.setCount(word.toLowerCase, 1.0)
          }
        }
      }

      // Answer candidates
      for (answerCandidate <- question.question.choices)
      for (sent <- answerCandidate.annotation.sentences) {
        for (word <- sent.words) {
          if (word.length > 0) {
            rowCounter.setCount(word.toLowerCase, 1.0)
          }
        }
      }

      // Add row counter to global counter
      freq += rowCounter
    }

    // Return
    freq
  }


  // Term frequency from one question (question terms only, no answer terms)
  def mkTermFreqQuestion(question:MCExplQuestion, onlyContentTags:Boolean = false):Counter[String] = {
    val freq = new Counter[String]

    // Question
    for (sent <- question.question.annotation.sentences) {
      val words = sent.words
      val tags = sent.tags.get
      for (i <- 0 until words.size) {
        var tag = tags(i)
        if (tag.length > 2) tag = tag.slice(0, 2)
        if ((contentTags.contains(tag)) || (onlyContentTags == false)) {
          if (words(i).length > 0) {
            freq.incrementCount(words(i).toLowerCase, 1.0)
          }
        }
      }
    }

    // Return
    freq
  }

  // Term frequency from one answer candidate (i.e. multiple choice answer candidate)
  def mkTermFreqAnswerCandidate(question:MCExplQuestion, choiceNum:Int, onlyContentTags:Boolean = false):Counter[String] = {
    val freq = new Counter[String]

    // Question
    for (sent <- question.question.choices(choiceNum).annotation.sentences) {
      val words = sent.words
      val tags = sent.tags.get
      for (i <- 0 until words.size) {
        var tag = tags(i)
        if (tag.length > 2) tag = tag.slice(0, 2)
        if ((contentTags.contains(tag)) || (onlyContentTags == false)) {
          if (words(i).length > 0) {
            freq.incrementCount(words(i).toLowerCase, 1.0)
          }
        }
      }
    }

    // Return
    freq
  }

  // Term frequency from one tablestore row
  def mkTermFreqRow(row: TableRow, onlyDataColumns: Boolean = true): Counter[String] = {
    val freq = new Counter[String]
    val dataColIdxs = row.dataColumns

    for (idx <- dataColIdxs) {
      val cellStr = row.cells(idx).toLowerCase.trim().replaceAll("\\s+", " ")
      val cellWords = cellStr.split(" ")

      for (word <- cellWords) {
        if (word.length > 0) {
          freq.incrementCount(word, 1.0)
        }
      }
    }

    // Return
    freq
  }


  // tf.idf using LNN weighting (see Manning's IR course, lecture 6)
  def tfidfLNN(termFreq:Counter[String]):Counter[String] = {
    val lnnVector = new Counter[String]

    for (key <- termFreq.keySet) {
      val tf = termFreq.getCount(key)
      val value = (1 + math.log(tf))
      lnnVector.setCount(key, value)
    }

    // Return
    lnnVector
  }

  // tf.idf using LTN weighting (see Manning's IR course, lecture 6)
  def tfidfLTN(termFreq:Counter[String], docFreq:Counter[String], tablestore:TableStore):Counter[String] = {
    val numDocs = tablestore.numRows
    val ltnVector = new Counter[String]

    for (key <- termFreq.keySet) {
      var df = docFreq.getCount(key)
      if (df == 0) df = 1   // smoothing for zero-frequency (i.e. unobserved) items
      val tf = termFreq.getCount(key)
      val value = (1 + math.log(tf)) * math.log(numDocs / df)
      ltnVector.setCount(key, value)
    }

    // Return
    ltnVector
  }



  /*
   * Feature generation
   */

  def evaluateExplRowPoolTFIDF(question:MCExplQuestion, answerCandidate:Int, tablestore:TableStore, explRowPool:ExplRowPool): Unit = {
    val docFreq = mkDocFreqRows(tablestore)

    // Query vector
    val queryVecQ = tfidfLTN( mkTermFreqQuestion(question, onlyContentTags = true), docFreq, tablestore )
    val queryVecA = tfidfLTN( mkTermFreqAnswerCandidate(question, answerCandidate, onlyContentTags = true), docFreq, tablestore )

    // For each possible tablestore row, generate cosine similarity values comparing the row text with the text of the question and answer
    for (rowEval <- explRowPool.rowEvals) {
      // Row vector
      val row = rowEval.row
      val rowVecLNN = tfidfLNN( mkTermFreqRow( row, onlyDataColumns = true) )

      /*
      println ("question: " + question.question.text)
      println ("queryVecQ: " + queryVecQ)
      println ("queryVecA: " + queryVecA)
      println ("rowVecLNN: " + rowVecLNN)
      */

      // Compute cosine similarity between (question, row) and (answer, row)
      val cosQ = Counters.cosine(queryVecQ, rowVecLNN)
      val cosA = Counters.cosine(queryVecA, rowVecLNN)

      // Add cosine similarity values as a feature for this row
      rowEval.features.setCount("COS_Q", cosQ)
      rowEval.features.setCount("COS_A", cosA)
    }

  }



  /*
   * Explanation Row Preparation
   */

  // Prepare a storage class for a given question and answer candidate, with a blank set of evaluations for
  // each tablestore row.
  def mkBlankExplRowPool(question:MCExplQuestion, answerCandidate:Int, tablestore:TableStore):ExplRowPool = {
    // Initialize storage class
    val explRowPool = new ExplRowPool(question, answerCandidate, tablestore)

    // Populate pool with tablestore rows, with blank evaluations (so these can be evaluted by various metrics, e.g. tf.idf)
    for (table <- tablestore.tables) {
      for (row <- table.rows) {
        explRowPool.addTablestoreRow( row )
      }
    }

    // Return
    explRowPool
  }


  /*
   * Classifier: Ranking classifier mechanics (feature generation)
   */
  def populateFeatures(question:MCExplQuestion, enabledFeatures:Set[String], tablestore:TableStore):ExplRowPool = {
    val features = new Counter[String]
    val correctIdx = question.question.correctAnswer

    // Step 1: Generate a blank "explanation pool" -- a list of tablestore sentences that we can rank
    val explPool = mkBlankExplRowPool(question, correctIdx, tablestore)
    if (correctIdx < 0) return explPool     // Special case checking: If the question is invalid for whatever reason, return a blank pool.

    // Step 2: Feature generation: perform explanation row evaluation through various means for ranking
    if (enabledFeatures.contains("TFIDF")) {
      evaluateExplRowPoolTFIDF(question, correctIdx, tablestore, explPool)
    }

    if (enabledFeatures.contains("NEWFEATURE1")) {
      //evalNewFeature1(question, correctIdx, tablestore, explPool)
    }

    if (enabledFeatures.contains("NEWFEATURE2")) {
      //evalNewFeature1(question, correctIdx, tablestore, explPool)
    }

    // Return
    explPool
  }


  def mkDataset(questions:Array[MCExplQuestion], enabledFeatures:Set[String], tablestore:TableStore, relabelZeroFeaturePositives:Boolean):(RVFRankingDataset[String], Array[ExplRowPool]) = {
    val dataset = new RVFRankingDataset[String]
    val explPools = new Array[ExplRowPool](questions.length)

    for(i <- 0 until questions.size) {
      val question = questions(i)

      // Debug: Display Question
      logger.debug ("-----------------------------------------------------------------------------------")
      logger.debug ("Question idx: " + i + " / " + questions.size)
      logger.debug ("Question: " + question)


      // Step 1: Generate and populate features
      var explRowPool:ExplRowPool = populateFeatures(question, enabledFeatures, tablestore)
      explPools(i) = explRowPool

      // Step 2: Extract parallel (label, feature) arrays
      val (labels, features) = explRowPool.mkLabelFeaturePairs(relabelZeroFeaturePositives)

      // Step 3: Convert to 'datums' to use built-in ML frameworks in Processors
      val queryDatums = new ListBuffer[Datum[Int, String]]
      for (j <- 0 until labels.length) {
        val datum = new RVFDatum[Int, String](labels(j), features(j))
        queryDatums += datum
        //##logger.debug("rowIdx: " + j + " \tlabel: " + datum.label + " \t" + datum.featuresCounter)    // debug
      }

      dataset += queryDatums

      logger.debug("--------------------------------------------------------------")
    }

    // Return
    (dataset, explPools)
  }


  /*
   * Classifier: Ranking classifier mechanics (train/evaluation functions)
   */

  def train(classifier:RankingClassifier[String], questions:Array[MCExplQuestion], enabledFeatures:Set[String], tablestore:TableStore): Unit = {
    // Step 1: make dataset
    val (dataset, _) = mkDataset(questions, enabledFeatures, tablestore, relabelZeroFeaturePositives = true)

    // Step 2: Train classifier
    logger.info("Training... ")
    classifier.train(dataset)

    // Step 3: Display weights
    classifier match {
      case c:SVMRankingClassifier[String] => c.displayModel(new PrintWriter(System.out, true))
      case _ => println ("Train: Unknown classifier type.  Unable to display weights. ")
    }

    logger.info("End of training...")
  }


  def evaluate(classifier:RankingClassifier[String], questions:Array[MCExplQuestion], enabledFeatures:Set[String], tablestore:TableStore): Array[ExplRowPool] = {
    val gamma:Double = 1.0
    var numSamples:Double = 0
    var sumScores = new Counter[String]

    // Step 1: Make dataset
    val (dataset, explRowPools) = mkDataset(questions, enabledFeatures, tablestore, relabelZeroFeaturePositives = false)

    val errorsEncountered = new ArrayBuffer[Int]

    // Step 2: Generate predictions
    for (i <- 0 until dataset.size) {
      // Generate model features for test question
      val queryDatums = dataset.mkQueryDatums(i)

      // Generate scores for each row
      val rowScores = classifier.scoresOf(queryDatums).toArray
      //println( "Length of scores: " + rowScores.length)

      // Populate these externally-generated row scores into the row evaluation storage class
      explRowPools(i).populateExternalScores(rowScores)

      // Rank based on scores
      explRowPools(i).rank()

      // Scoring
      // Score the explanation regeneration task for this question
      val scores = explRowPools(i).getScores()
      println ("Scores: " + scores)

      // Debug printout
      println ("----------------------------------------------")
      println (questions(i).toString())
      println (explRowPools(i).toString())
      println ("----------------------------------------------")

      // Check for errors/infinities in the scores
      breakable {
        // Check for errors/infinities
        for (key <- scores.keySet) {
          val value = scores.getCount(key)
          if ((value == Double.NaN) || (value == Double.PositiveInfinity) || (value == Double.NegativeInfinity)) {
            scores.setCount(key, 0.0)
            //break()   // Found -- exit, without recording scores

            println ("ERROR/INFINITY/NAN!")
            errorsEncountered.append(i)
          }
        }
        // Not found -- record scores
        sumScores += scores
        numSamples += 1
      }

    }

    // Report scores
    println ("evaluate(): ")

    println ("Summary:   (n = " + numSamples + ")")
    println (summaryAverageScores(sumScores, numSamples))

    println ("")
    println ("Errors encounterd with nan/invalid scores: " + errorsEncountered.length + " (" + errorsEncountered.mkString(", ") + ")")

    // Return
    explRowPools
  }


  /*
   * Write predictions to a file
   *
   * The format is questionID<TAB>explanationID without header; the order is important
   */
  def write(explRowPools: Array[ExplRowPool], writer: PrintWriter): Unit = {
    explRowPools.foreach { explRowPool =>
      explRowPool.rowEvals.foreach { rowEval =>
        writer.format(Locale.ROOT, "%s\t%s%n", explRowPool.question.question.questionID, rowEval.row.uid)
      }
    }
  }

  /*
   * Score reporting
   */
  def summaryAverageScores(sumScores:Counter[String], numSamples:Double):String = {
    val os = new StringBuilder

    // Step 1: Average the scores
    val avgScores = new Counter[String]
    for (key <- sumScores.keySet) {
      var average:Double = 0.0f
      average = sumScores.getCount(key) / numSamples
      avgScores.setCount(key, average)
    }

    println( avgScores.sorted(descending = true) )

    // Step 2: Display scores in string
    os.append("-------------------------------------------------\n")
    os.append(" Performance on Evaluation Set\n")
    os.append("-------------------------------------------------\n")
    os.append("\n")

    os.append("MAP: " + avgScores.getCount(SCORE_MAP).formatted("%3.4f") + "\n")
    os.append("\n")
    os.append("MAP_ROLE_CENTRAL:    " + avgScores.getCount(SCORE_MAP_CENTRAL).formatted("%3.4f") + "\n")
    os.append("MAP_ROLE_GROUNDING:  " + avgScores.getCount(SCORE_MAP_GROUNDING).formatted("%3.4f") + "\n")
    os.append("MAP_ROLE_LEXGLUE:    " + avgScores.getCount(SCORE_MAP_LEXGLUE).formatted("%3.4f") + "\n")
    os.append("MAP_ROLE_BACKGROUND: " + avgScores.getCount(SCORE_MAP_BACKGROUND).formatted("%3.4f") + "\n")
    os.append("\n")
    os.append("MAP_LEXOVERLAP:      " + avgScores.getCount(SCORE_MAP_LEXOVERLAP).formatted("%3.4f") + "\n")
    os.append("MAP_NOLEXOVERLAP:    " + avgScores.getCount(SCORE_MAP_NOLEXOVERLAP).formatted("%3.4f") + "\n")
    os.append("\n")
    os.append("MAP_TABKT_RET:       " + avgScores.getCount(SCORE_MAP_TABKT_RET).formatted("%3.4f") + "\n")
    os.append("MAP_TABKT_RET/LEX:   " + avgScores.getCount(SCORE_MAP_TABKT_RETLEX).formatted("%3.4f") + "\n")
    os.append("MAP_TABKT_INFSUPP:   " + avgScores.getCount(SCORE_MAP_TABKT_INFSUPP).formatted("%3.4f") + "\n")
    os.append("MAP_TABKT_COMPLEX:   " + avgScores.getCount(SCORE_MAP_TABKT_COMPLEX).formatted("%3.4f") + "\n")
    os.append("\n")
    os.append("Precision@1          " + avgScores.getCount(SCORE_PRECISIONAT1).formatted("%3.4f") + "\n")
    os.append("Precision@2          " + avgScores.getCount(SCORE_PRECISIONAT2).formatted("%3.4f") + "\n")
    os.append("Precision@3          " + avgScores.getCount(SCORE_PRECISIONAT3).formatted("%3.4f") + "\n")
    os.append("Precision@4          " + avgScores.getCount(SCORE_PRECISIONAT4).formatted("%3.4f") + "\n")
    os.append("Precision@5          " + avgScores.getCount(SCORE_PRECISIONAT5).formatted("%3.4f") + "\n")
    os.append("\n")

    // Return
    os.toString()
  }

  def summaryAverageScores2(sumScores:Counter[String], sampleCounts:Counter[String]):String = {
    val os = new StringBuilder

    // Step 1: Average the scores
    val avgScores = new Counter[String]
    for (key <- sumScores.keySet) {
      var average:Double = 0.0f
      val numSamples = sampleCounts.getCount(key)
      average = sumScores.getCount(key) / numSamples
      avgScores.setCount(key, average)
    }

    println( "avgScores: " + avgScores.sorted(descending = true) )
    println ( "sampleCounts: " + sampleCounts.sorted(descending = true) )

    // Step 2: Display scores in string
    os.append("-------------------------------------------------\n")
    os.append(" Performance on Evaluation Set\n")
    os.append("-------------------------------------------------\n")
    os.append("\n")

    os.append("MAP: " + avgScores.getCount(SCORE_MAP).formatted("%3.4f") + "\n")
    os.append("\n")
    os.append("MAP_ROLE_CENTRAL:     " + avgScores.getCount(SCORE_MAP_CENTRAL).formatted("%3.4f") + "\n")
    os.append("MAP_ROLE_GROUNDING:   " + avgScores.getCount(SCORE_MAP_GROUNDING).formatted("%3.4f") + "\n")
    os.append("MAP_ROLE_LEXGLUE:     " + avgScores.getCount(SCORE_MAP_LEXGLUE).formatted("%3.4f") + "\n")
    os.append("MAP_ROLE_BACKGROUND:  " + avgScores.getCount(SCORE_MAP_BACKGROUND).formatted("%3.4f") + "\n")
    os.append("\n")
    os.append("MAP_LEXOVERLAP1WOnly: " + avgScores.getCount(SCORE_MAP_LEXOVERLAP1W).formatted("%3.4f") + "\n")
    os.append("MAP_LEXOVERLAP2+W:    " + avgScores.getCount(SCORE_MAP_LEXOVERLAP2PW).formatted("%3.4f") + "\n")
    os.append("MAP_LEXOVERLAP:       " + avgScores.getCount(SCORE_MAP_LEXOVERLAP).formatted("%3.4f") + "\n")
    os.append("MAP_NOLEXOVERLAP:     " + avgScores.getCount(SCORE_MAP_NOLEXOVERLAP).formatted("%3.4f") + "\n")

    os.append("\n")
    os.append("MAP_TABKT_RET:        " + avgScores.getCount(SCORE_MAP_TABKT_RET).formatted("%3.4f") + "\n")
    os.append("MAP_TABKT_RET/LEX:    " + avgScores.getCount(SCORE_MAP_TABKT_RETLEX).formatted("%3.4f") + "\n")
    os.append("MAP_TABKT_INFSUPP:    " + avgScores.getCount(SCORE_MAP_TABKT_INFSUPP).formatted("%3.4f") + "\n")
    os.append("MAP_TABKT_COMPLEX:    " + avgScores.getCount(SCORE_MAP_TABKT_COMPLEX).formatted("%3.4f") + "\n")
    os.append("\n")
    os.append("Precision@1           " + avgScores.getCount(SCORE_PRECISIONAT1).formatted("%3.4f") + "\n")
    os.append("Precision@2           " + avgScores.getCount(SCORE_PRECISIONAT2).formatted("%3.4f") + "\n")
    os.append("Precision@3           " + avgScores.getCount(SCORE_PRECISIONAT3).formatted("%3.4f") + "\n")
    os.append("Precision@4           " + avgScores.getCount(SCORE_PRECISIONAT4).formatted("%3.4f") + "\n")
    os.append("Precision@5           " + avgScores.getCount(SCORE_PRECISIONAT5).formatted("%3.4f") + "\n")
    os.append("Precision@10          " + avgScores.getCount(SCORE_PRECISIONAT10).formatted("%3.4f") + "\n")
    os.append("Precision@20          " + avgScores.getCount(SCORE_PRECISIONAT20).formatted("%3.4f") + "\n")
    os.append("\n")

    // Return
    os.toString()
  }



  /*
   * Supporting functions
   */
  // Converts between question storage classes (one that contains better segmented explanation information)
  def convertToExplQuestions(in:Array[MCQuestion]):Array[MCExplQuestion] = {
    val out = new ArrayBuffer[MCExplQuestion]
    for (i <- 0 until in.size) {
      out.append( new MCExplQuestion(in(i)) )
    }
    // Return
    out.toArray
  }

  // Take a set of questions, and return only the questions that have a non-zero value on one or more flags.
  def filterQuestionsByFlags(in:Array[MCExplQuestion], flags:Array[String]):Array[MCExplQuestion] = {
    val out = new ArrayBuffer[MCExplQuestion]

    for (i <- 0 until in.size) {
      breakable {
        for (flag <- flags) {
          if (in(i).question.flags.getCount(flag) > 0) {
            out.append(in(i))
          }
        }
      }
    }

    // Return
    out.toArray
  }


  /*
   * Distribution of tables across explanations (e.g. taxonomic, 76%, synonymy, 56%, ...)
   * This is just for helpful summary statistics to the user, and isn't critical to running the experiment.
   */
  def calculateTableUseSummary(in:Array[MCExplQuestion], tablestore:TableStore): Unit = {
    var numWithExpl:Double = 0.0
    var explLength:Double = 0.0

    var tableUse = new Counter[String]

    // For each question
    for (i <- 0 until in.size) {
      val explQuestion = in(i)
      val question = explQuestion.question
      val expl = in(i).expl

      // Check if it has an explanation populated
      if (expl.size > 0) {
        // Explanations and Explanation Length
        numWithExpl += 1
        explLength += expl.size

        val tableUseOneQuestion = new Counter[String]

        // Explanation sentences/table rows
        for (j <- 0 until expl.size) {
          val explSent = expl(j)
          val role = explSent.role
          val uid = explSent.uid

          // Store which table this UID/table row/explanation sentence came from
          val tableIdx = tablestore.UIDtoTableLUT(uid)
          if (tableIdx >= 0) {
            val tableName = tablestore.tables(tableIdx).name
            tableUseOneQuestion.setCount(tableName, 1.0)
            //## Debug/verbose mode
            // println (tableName + "\t" + uid)
          } else {
            println ("ERROR: UID not found: " + uid)
          }

        }

        // Add a given question's (binary) counter to the global counter
        for (key <- tableUseOneQuestion.keySet) {
          tableUse.incrementCount(key, 1.0)
        }

      } else {
        // No explanation populated

      }
    }


    // Display counter
    println ("")

    println ("Knowledge Use Summary")

    println ("")
    println ("Prop.\tReuse\tTableName")
    val sorted = tableUse.sorted(descending = true)
    for (i <- 0 until sorted.size) {
      val proportion = sorted(i)._2.toDouble / numWithExpl
      val label = sorted(i)._1

      var reuseProportion:Double = 0.0
      val tableIdx = tablestore.findTableIdxByName(label)
      if (tableIdx.isDefined) {
        val numRows:Double = tablestore.tables(tableIdx.get).numRows()
        reuseProportion = (numWithExpl * proportion) / numRows
        //println (numRows + "\t" + numWithExpl)
      }

      println (proportion.formatted("%3.3f") + "\t" + reuseProportion.formatted("%3.1f") + " \t" + label )
    }

  }




  /*
   * Prints the command line usage information to the console
   */
  def printUsage() {
    println (PROGRAM_TITLE)
    println ("")
    println ("Usage: ... -props myprops.properties")
    println ("")
  }


  /*
   * Main entry point
   */
  def main(args:Array[String]) {
    // Step 1: Check that command line arguments were specified for running this program
    // e.g. " -props myprops.properties"
    if ((args.length == 0)) {
      printUsage()
      System.exit(1)
    }
    // Parse the command line arguments, and place this in a storage class called 'props'
    val props = StringUtils.argsToProperties(args)


    // Step 2: Load the tablestore that we need to run the experiments

    // Step 2A: Find tablestore index filename from the properties list
    // The tablestore is loaded by referencing a file that includes a list of tables, called the tablestore index.
    // Load the name of the tablestore index file from the properties file.
    var tablestoreIndex:String = ""
    if (props.getProperty("tablestoreIndex", "") != "") {
      tablestoreIndex = props.getProperty("tablestoreIndex", "")
    } else {
      throw new RuntimeException("ERROR: Unable to find 'tablestoreIndex' property in properties file.")
    }

    // Step 2B: Load the tablestore
    val tablestore = new TableStore(tablestoreIndex)

    //## Debug statements -- test usage of the tablestore
    println ( tablestore.tables( tablestore.UIDtoTableLUT("a5c9-d7a4-8421-bb2e") ).name )
    println ( tablestore.getRowByUID("a5c9-d7a4-8421-bb2e") )



    // Step 3: Load the questions file that also includes the gold explanation graph data
    // Step 3A: Find question filename from the properties file
    var filenameQuestionsTrain:String = ""
    var filenameQuestionsEval:String = ""

    if (props.getProperty("questionsTrain", "") != "") {
      filenameQuestionsTrain = props.getProperty("questionsTrain", "")
    } else {
      throw new RuntimeException("ERROR: Unable to find 'questionsTrain' property in properties file.")
    }

    if (props.getProperty("questionsEval", "") != "") {
      filenameQuestionsEval = props.getProperty("questionsEval", "")
    } else {
      throw new RuntimeException("ERROR: Unable to find 'questionsEval' property in properties file.")
    }


    // Step 3B: Load questions using question parser
    var questionsTrain = ExamQuestionParserDynamic.loadQuestionsFromCSVList( filenameQuestionsTrain, fullAnnotation = false, noAnnotation = false, tsvMode = true)
    var questionsEval = ExamQuestionParserDynamic.loadQuestionsFromCSVList( filenameQuestionsEval, fullAnnotation = false, noAnnotation = false, tsvMode = true)
    // Step 3C: Convert questions from MCQuestions to a storage class that includes the explanation for the question, MCExplQuestion
    var explQuestionsTrain = convertToExplQuestions(questionsTrain)
    var explQuestionsEval = convertToExplQuestions(questionsEval)

    //## Debug: show that we've loaded the first 10 questions successfully
    println ("Displaying first 10 questions (debug): ")
    for (i <- 0 until 10) {
      println (explQuestionsTrain(i).toString)
      println ("")
    }

    // Step 3D: Filter questions to only those that have been tagged as successfully annotated
    var filteredQuestionsTrain = filterQuestionsByFlags(explQuestionsTrain, Array("SUCCESS", "READY") )
    var filteredQuestionsEval = filterQuestionsByFlags(explQuestionsEval, Array("SUCCESS", "READY") )
    println ("Loaded " + filteredQuestionsTrain.size + " training questions after filtering. ")
    println ("Loaded " + filteredQuestionsEval.size + " evaluation questions after filtering. ")

    // If debugging, allow the user to limit the number of questions trained/evaluated for fast runtimes/iterations.
    val debugQuestionLimit = StringUtils.getInt(props, "debug.limitNumQuestions", 0)
    if (debugQuestionLimit > 0) {
      println ("'debug.limitNumQuestions' property is enabled.  Limiting number of training and evaluation questions to " + debugQuestionLimit)
      if (debugQuestionLimit > filteredQuestionsTrain.length) throw new RuntimeException("ERROR: debug.limitNumQuestions (" + debugQuestionLimit + ") must be less than the number of training questions (" + filteredQuestionsTrain.length + ")")
      filteredQuestionsTrain = filteredQuestionsTrain.slice(0, debugQuestionLimit)
      if (debugQuestionLimit > filteredQuestionsEval.length) throw new RuntimeException("ERROR: debug.limitNumQuestions (" + debugQuestionLimit + ") must be less than the number of evaluation questions (" + filteredQuestionsEval.length + ")")
      filteredQuestionsEval = filteredQuestionsEval.slice(0, debugQuestionLimit)
    }


    // Step 4: Compute The proportion of explanations that contain knowledge from a given table (Table 3 in LREC 2018 paper)
    // Note: This is not important to this experiment, and is just an informative repeat of one of the analyses in the LREC2018 paper.
    calculateTableUseSummary(filteredQuestionsTrain, tablestore)
    println ("\n\n")


    // Step 5: Enabled features
    // Retrieve a list of enabled features from the properties file
    val enabledFeatures = props.getProperty("enabledFeatures").toUpperCase().split(",").map(_.trim).toSet


    // Step 6: Generate classifier
    // SVM Ranking classifier download: https://www.cs.cornell.edu/people/tj/svm_light/svm_rank.html
    // Must be placed in an area on the PATH environment variable to be executed by Processors.

    val classifierProperties = new Properties()
    val workunitID = 1
    val svm_c = props.getProperty("ranker.svm_c", "1.0") // extract C parameter from properties file
    classifierProperties.put("c", svm_c)
    classifierProperties.put("debugFile", "./debug-features-w" + workunitID)
    classifierProperties.setProperty("modelFile", "model" + workunitID + ".dat")
    classifierProperties.setProperty("trainFile", "train" + workunitID + ".dat")
    classifierProperties.setProperty("workingDir", ".")
    val classifier = new SVMRankingClassifier[String](classifierProperties)


    // Step 7: Train classifier
    train(classifier, filteredQuestionsTrain, enabledFeatures, tablestore)

    // Step 8: Evaluate classifier performance
    val explRowPools = evaluate(classifier, filteredQuestionsEval, enabledFeatures, tablestore)
    println ("EnabledFeatures: " + enabledFeatures.toList.sorted.mkString(", "))

    // Step 9: (Optional) Write predictions to a file
    if (props.containsKey("predictOutput")) {
      val file = new File(props.getProperty("predictOutput"))
      println("Writing predictions to " + file.getAbsolutePath)

      val writer = new PrintWriter(file)
      write(explRowPools, writer)

      writer.close()
    } else {
      println("Not writing predictions to a file. Set the predictOutput property, if needed.")
    }

    // Step 2: (Optional) Display model weights
    println("-------------------------------------------------\n")
    println(" Model Weights\n")
    println("-------------------------------------------------\n")
    println("\n")

    classifier.displayModel(new PrintWriter(System.out, true))

  }




}