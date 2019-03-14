package explanationregeneration

import java.io.PrintWriter
import java.util.Properties

import data.question.{ExamQuestionParserDynamic, MCExplQuestion, MCQuestion}
import edu.arizona.sista.struct.{Counter, Counters}
import edu.arizona.sista.utils.StringUtils
import explanationgraph.{TableRow, TableStore}
import ExplRowPool._
import edu.arizona.sista.learning._
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

    // Compare with cosine similarity of each row vector
    for (rowEval <- explRowPool.rowEvals) {
      val row = rowEval.row
      val rowVecLNN = tfidfLNN( mkTermFreqRow( row, onlyDataColumns = true) )

      /*
      println ("question: " + question.question.text)
      println ("queryVecQ: " + queryVecQ)
      println ("queryVecA: " + queryVecA)
      println ("rowVecLNN: " + rowVecLNN)
      */

      val cosQ = Counters.cosine(queryVecQ, rowVecLNN)
      val cosA = Counters.cosine(queryVecA, rowVecLNN)

      rowEval.features.setCount("COS_Q", cosQ)
      rowEval.features.setCount("COS_A", cosA)

      //println( rowEval.evals )
      //println ("")
    }

  }



  def evaluateExplRowPoolQC(question:MCExplQuestion, answerCandidate:Int, tablestore:TableStore, explRowPool:ExplRowPool, qcRowModel:QCExplanationRowPredictor, extraLabelText:String = ""): Unit = {
    val qcLabels = question.question.topic
    val numLabels = qcLabels.length

    // Step 1: Grab all the row scores for each of the QC labels for this question
    val rowScores = new Array[Array[RowScore]](numLabels)
    for (labelIdx <- 0 until numLabels) {
      rowScores(labelIdx) = qcRowModel.queryCategory(qcLabels(labelIdx))
    }

    // For each row in the tablestore
    for (rowEval <- explRowPool.rowEvals) {
      val row = rowEval.row

      // For each QC label for this question
      for (labelIdx <- 0 until numLabels) {

        breakable {
          // Grab the list of QC-related scores for each row
          val rowScores_ = rowScores(labelIdx)

          // Search for the row in the QC scores
          for (i <- 0 until rowScores_.length) {
            val rowDist = rowScores_(i).rowDist
            val scoreThisQC = rowScores_(i).score

            // Once the row is found
            if (rowDist.row == row) {
              // Record the score
              val currentScore = rowEval.features.getCount("QC" + extraLabelText)
              val newScore = math.max(currentScore, scoreThisQC)
              rowEval.features.setCount("QC" + extraLabelText, newScore)
              //println("FOUND: " + rowEval.evals )
              break()     // We found the row evaluation in the rowScores array -- so we don't need to keep iterating through the rest of the array -- break.
            }
          }
        }
      }
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
  def populateFeatures(question:MCExplQuestion, enabledFeatures:Set[String], tablestore:TableStore, qcRowProbGroup:QCRowProbGroup):ExplRowPool = {
    val features = new Counter[String]
    val correctIdx = question.question.correctAnswer

    // Generate a blank "explanation pool" -- a list of tablestore sentences that we can rank
    val explPool = mkBlankExplRowPool(question, correctIdx, tablestore)
    if (correctIdx < 0) return explPool     // Special case checking: If the question is invalid for whatever reason, return a blank pool.

    // Feature generation: perform explanation row evaluation through various means for ranking
    if (enabledFeatures.contains("TFIDF")) {
      evaluateExplRowPoolTFIDF(question, correctIdx, tablestore, explPool)
    }

    if (enabledFeatures.contains("QC")) {
      for (level <- 0 until qcRowProbGroup.maxLevel) {
        evaluateExplRowPoolQC(question, correctIdx, tablestore, explPool, qcRowProbGroup.getAtLevel(level), "_L" + level)
      }
    }

    if (enabledFeatures.contains("QC_SINGLE")) {
      evaluateExplRowPoolQC(question, correctIdx, tablestore, explPool, qcRowProbGroup.getAtLevel(0), "_L0")
    }

    if (enabledFeatures.contains("QC_SINGLE_L1")) {
      evaluateExplRowPoolQC(question, correctIdx, tablestore, explPool, qcRowProbGroup.getAtLevel(1), "_L1")
    }

    if (enabledFeatures.contains("QC_SINGLE_L2")) {
      evaluateExplRowPoolQC(question, correctIdx, tablestore, explPool, qcRowProbGroup.getAtLevel(2), "_L2")
    }

    if (enabledFeatures.contains("QC_SINGLE_L3")) {
      evaluateExplRowPoolQC(question, correctIdx, tablestore, explPool, qcRowProbGroup.getAtLevel(3), "_L3")
    }

    if (enabledFeatures.contains("QC_SINGLE_L4")) {
      evaluateExplRowPoolQC(question, correctIdx, tablestore, explPool, qcRowProbGroup.getAtLevel(4), "_L4")
    }

    if (enabledFeatures.contains("QC_SINGLE_L5")) {
      evaluateExplRowPoolQC(question, correctIdx, tablestore, explPool, qcRowProbGroup.getAtLevel(5), "_L5")
    }

    if (enabledFeatures.contains("QC_SINGLE_L6")) {
      evaluateExplRowPoolQC(question, correctIdx, tablestore, explPool, qcRowProbGroup.getAtLevel(6), "_L6")
    }

    if (enabledFeatures.contains("QC_SINGLE_L7")) {
      evaluateExplRowPoolQC(question, correctIdx, tablestore, explPool, qcRowProbGroup.getAtLevel(7), "_L7")
    }

    if (enabledFeatures.contains("QC_SINGLE_L8")) {
      evaluateExplRowPoolQC(question, correctIdx, tablestore, explPool, qcRowProbGroup.getAtLevel(8), "_L8")
    }

    // Return
    explPool
  }


  def mkDataset(questions:Array[MCExplQuestion], enabledFeatures:Set[String], tablestore:TableStore, qcRowProbGroup:Option[QCRowProbGroup], relabelZeroFeaturePositives:Boolean):(RVFRankingDataset[String], Array[ExplRowPool]) = {
    val dataset = new RVFRankingDataset[String]
    val explPools = new Array[ExplRowPool](questions.length)

    for(i <- 0 until questions.size) {
      val question = questions(i)

      // Debug: Display Question
      logger.debug ("-----------------------------------------------------------------------------------")
      logger.debug ("Question idx: " + i + " / " + questions.size)
      logger.debug ("Question: " + question)


      // Step 1: Generate and populate features
      var explRowPool:ExplRowPool = null

      var qcEnabled:Boolean = false
      for (featureName <- enabledFeatures) {
        if (featureName.startsWith("QC")) qcEnabled = true
      }

      if (qcEnabled == false) {
        val blankGroup = new QCRowProbGroup(tablestore, 1)        // Create faux blank group (not used)
        explRowPool = populateFeatures(question, enabledFeatures, tablestore, blankGroup)

      } else {
        // Handle QC Caching (for speed)
        if (qcRowProbGroup.isDefined) {
          // Evaluation: The pool is static for each question, and externally supplied.
          explRowPool = populateFeatures(question, enabledFeatures, tablestore, qcRowProbGroup.get)
        } else {
          // Training: The pool changes for each question, using leave-one-out crossvalidation to reduce overfitting.
          // Either load a cached version of the pool, or generate a pool using leave-one-out crossvalidation if one can't be loaded.
          val questionsSliced = questions.slice(0, i) ++ questions.slice(i + 1, questions.size)
          var qcRowProbGroup_LOOXV: QCRowProbGroup = null

          // Check if we can load a pre-cached version of the pool, with this question left out
          // TODO: Load the QC Cache path from properties file
          val filenamePrefix = "/data/qccache/qs" + questions.length + "-qid" + question.question.questionID
          if (QCRowProbGroup.canLoadFromFiles(filenamePrefix, tablestore, maxLevel = 8)) {
            // A set of cached leave-one-out-crossvalidation pools exist, load them from file
            println("Loading cached pool... (" + filenamePrefix + ")")
            qcRowProbGroup_LOOXV = QCRowProbGroup.loadFromFiles(filenamePrefix, tablestore, maxLevel = 8)
          } else {
            // No cached pool found -- generate, and save
            println("Generating pool... ")
            qcRowProbGroup_LOOXV = QCRowProbGroup.mkQCRowProbGroup(questionsSliced, tablestore, maxLevel = 8)
            println("Saving cached pool... (" + filenamePrefix + ")")
            qcRowProbGroup_LOOXV.saveToFiles(filenamePrefix)
          }

          explRowPool = populateFeatures(question, enabledFeatures, tablestore, qcRowProbGroup_LOOXV)
        }
      }

      explPools(i) = explRowPool
      // Step 2: extract parallel (label, feature) arrays
      val (labels, features) = explRowPool.mkLabelFeaturePairs(relabelZeroFeaturePositives)

      // Step 3: Convert to datums
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

  def train(classifier:RankingClassifier[String], questions:Array[MCExplQuestion], enabledFeatures:Set[String], tablestore:TableStore, qcRowProbGroup:QCRowProbGroup): Unit = {
    // Step 1: make dataset
    val (dataset, _) = mkDataset(questions, enabledFeatures, tablestore, None, relabelZeroFeaturePositives = true)

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


  def evaluate(classifier:RankingClassifier[String], questions:Array[MCExplQuestion], enabledFeatures:Set[String], tablestore:TableStore, qcRowProbGroup:QCRowProbGroup): Unit = {
    val gamma:Double = 1.0
    var numSamples:Double = 0
    var sumScores = new Counter[String]

    // Step 1: make dataset
    val (dataset, explRowPools) = mkDataset(questions, enabledFeatures, tablestore, Some(qcRowProbGroup), relabelZeroFeaturePositives = false)

    // Step 2: Generate predictions
    for (i <- 0 until dataset.size) {
      // Generate model features for test question
      val queryDatums = dataset.mkQueryDatums(i)

      // Generate scores for each row
      val rowScores = classifier.scoresOf(queryDatums).toArray
      println( "Length of scores: " + rowScores.length)

      // Populate these externally-generated row scores into the row evaluation storage class
      explRowPools(i).populateExternalScores(rowScores)

      // Rank based on scores
      explRowPools(i).rank()

      // Scoring
      // Score the explanation regeneration task for this question
      val scores = explRowPools(i).getScores()
      println ("Scores: " + scores)

      // Check for errors/infinities in the scores
      breakable {
        // Check for errors/infinities
        for (key <- scores.keySet) {
          val value = scores.getCount(key)
          if ((value == Double.NaN) || (value == Double.PositiveInfinity) || (value == Double.NegativeInfinity)) {
            break()   // Found -- exit, without recording scores
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

  }



  /*
   * Score reporting
   */
  def summaryAverageScores(sumScores:Counter[String], numSamples:Double):String = {
    val os = new StringBuilder

    // Step 1: Average the scores
    val avgScores = new Counter[String]
    for (key <- sumScores.keySet) {
      val average = sumScores.getCount(key) / numSamples
      avgScores.setCount(key, average)
    }

    println( avgScores.sorted(descending = true) )


    // Step 2: Display scores in string
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


    // Step 5: Precompute explanatory role distribution
    // Precompute distribution of explanatory roles for each tablerow that appears in the first 100 explanations.
    // 'qcerp' is a storage class that for each table row, stores the distribution of roles that row is observed in in explanations.
    // For example:
    // QCRowDist: metal is a kind of material (KINDOF, UID: 95c4-ec5e-bce9-afd8)
    //   roleDist: GROUNDING:4.000  CENTRAL:1.000

    println ("* calling QCExplanationRowPredictor:")
    val qcerp = QCExplanationRowPredictor.mkQCExplanationRowPredictor( filteredQuestionsTrain, tablestore, heirLevel = -1)
    println ("Explanatory Role Distribution (debug):")
    qcerp.display(maxDisplay = 20)

    // Generate QCExplanationRowPredictor distribution across all levels of the heirarchy
    val qcRowProbGroup = QCRowProbGroup.mkQCRowProbGroup(filteredQuestionsTrain, tablestore, maxLevel = 8)

    // Enabled features

    // Retrieve a list of enabled features from the properties file
    val enabledFeatures = props.getProperty("enabledFeatures").toUpperCase().split(",").map(_.trim).toSet


    // Generate classifier
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


    /*
    // Perceptron
    val classifierProperties = new Properties()
    classifierProperties.setProperty("classifierClass", "PerceptronRankingClassifier")
    classifierProperties.setProperty("epochs", "10")
    classifierProperties.setProperty("burnInIterations", "5")
    val classifier = new PerceptronRankingClassifier[String](classifierProperties)
    */

    /*
    classifierProperties.setProperty("treesNumLeaves", "3")
    classifierProperties.setProperty("trainFraction", "0.90")
    val test = new JForestsRankingClassifier[String](classifierProperties)
    */



    // Train
    train(classifier, filteredQuestionsTrain, enabledFeatures, tablestore, qcRowProbGroup)

    // Evaluate
    evaluate(classifier, filteredQuestionsEval, enabledFeatures, tablestore, qcRowProbGroup)
    println ("EnabledFeatures: " + enabledFeatures.toList.sorted.mkString(", "))

    // Print model feature weights
    println ("Model: " + classifier.displayModel(new PrintWriter(System.out, true)))

  }




}