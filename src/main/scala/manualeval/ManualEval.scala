package manualeval

import data.question.{ExamQuestionParserDynamic, ExplanationRow, MCExplQuestion}
import edu.arizona.sista.struct.Counter
import edu.arizona.sista.utils.StringUtils
import explanationgraph.{LookupLemmatizer, TableKnowledgeCategories, TableRow, TableStore}
import explanationregeneration.{ExplRowPool, ExplanationRegeneration, RowEval}
import explanationregeneration.ExplanationRegeneration.{contentTags, convertToExplQuestions, filterQuestionsByFlags, summaryAverageScores}
import statistics.Statistics.{calculateExplanationLengthSummary, calculateExplanationRoleSummary, calculateTableUseSummary, printUsage}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

class ManualEval {

}


object ManualEval {


  def loadModelPredictions(filename: String): Map[String, ArrayBuffer[String]] = {
    val out = mutable.Map[String, ArrayBuffer[String]]()

    println(" * Loading model predictions: " + filename)
    for (line <- io.Source.fromFile(filename, "UTF-8").getLines()) {
      val fields = line.split("\t")
      val qid = fields(0).trim()
      val uuid = fields(1).trim().toLowerCase

      // Find qid
      if (!out.contains(qid)) {
        // If the key hasn't been used before, add a blank array
        out(qid) = new ArrayBuffer[String]
      }
      val ab = out(qid)
      ab.append(uuid)
      out(qid) = ab
    }

    println(" * Predictions loaded (number of keys: " + out.size + ")")

    // Return
    out.toMap
  }


  def displayModelPredictions(explQuestion: MCExplQuestion, tablestore: TableStore, models: Array[Map[String, ArrayBuffer[String]]]): Unit = {

    val question = explQuestion.question
    val expl = explQuestion.expl

    println("Question: ")
    println(explQuestion.toString())
    println("")


    for (i <- 0 until models.length) {
      println("Model " + i + " Rankings: ")

      val model = models(i)
      if (model.contains(question.questionID)) {
        val rankings = model(question.questionID)

        for (j <- 0 until math.min(rankings.length, 20)) {
          var isGold: Boolean = false
          breakable {
            for (a <- 0 until expl.length) {
              if (expl(a).uid.toLowerCase == rankings(j)) {
                isGold = true
                break()
              }
            }
          }

          if (isGold) {
            println(j + " : * " + rankings(j) + "\t" + tablestore.getRowByUID(rankings(j)).toStringText())
          } else {
            println(j + " :   " + rankings(j) + "\t" + tablestore.getRowByUID(rankings(j)).toStringText())
          }
        }
      } else {
        println("ERROR: Question ID not found: " + question.questionID)
      }

      println("")

    }

  }


  def displayModelPredictionsTSV(explQuestion: MCExplQuestion, tablestore: TableStore, models: Array[Map[String, ArrayBuffer[String]]]): Unit = {

    val question = explQuestion.question
    val expl = explQuestion.expl

    //println("Question: ")
    //println(explQuestion.toString())
    //println("")
    println("Question: " + question.text)
    for (i <- 0 until question.choices.length) {
      if (question.correctAnswer == i) {
        println("(" + i + ") * " + question.choices(i))
      } else {
        println("(" + i + ") " + question.choices(i))
      }
    }


    val delim = "\t"

    for (j <- 0 until 20) {
      val strOut = new StringBuilder()

      strOut.append(j + delim)

      for (i <- 0 until models.length) {
        //println("Model " + i + " Rankings: ")

        val model = models(i)
        if (model.contains(question.questionID)) {
          val rankings = model(question.questionID)

          var isGold: Boolean = false
          breakable {
            for (a <- 0 until expl.length) {
              if (expl(a).uid.toLowerCase == rankings(j)) {
                isGold = true
                break()
              }
            }
          }

          if (isGold) {
            strOut.append("1" + delim)
          } else {
            strOut.append("0" + delim)
          }

          strOut.append(tablestore.getRowByUID(rankings(j)).toStringSentWithUID("") + delim)

          strOut.append("" + delim)

        } else {
          strOut.append("0" + delim)
          strOut.append("" + delim)

          strOut.append("" + delim)
        }

      }

      println(strOut.toString)

    }

    println("")

  }


  def displayModelPredictionsSummaryTSV(explQuestion: MCExplQuestion, tablestore: TableStore, models: Array[Map[String, ArrayBuffer[String]]]): Unit = {
    val delim = "\t"
    val question = explQuestion.question
    val expl = explQuestion.expl

    println("Question: " + question.text)
    for (i <- 0 until question.choices.length) {
      if (question.correctAnswer == i) {
        println("(" + i + ") * " + question.choices(i))
      } else {
        println("(" + i + ") " + question.choices(i))
      }
    }

    println ("QID" + delim + question.questionID)

    val rows = mutable.Set[(Int, TableRow)]()
    var notFound:Boolean = false

    for (j <- 0 until 20) {
      for (i <- 0 until models.length) {
        //println("Model " + i + " Rankings: ")

        val model = models(i)
        if (model.contains(question.questionID)) {
          val rankings = model(question.questionID)

          var isGold:Int = 0
          breakable {
            for (a <- 0 until expl.length) {
              if (expl(a).uid.toLowerCase == rankings(j)) {
                isGold = 1
                break()
              }
            }
          }

          //strOut.append(tablestore.getRowByUID(rankings(j)).toStringSentWithUID("") + delim)
          rows.add( (isGold, tablestore.getRowByUID(rankings(j))) )

        } else {
          notFound = true
        }
      }
    }

    val sorted = rows.toArray.sortBy(-_._1)
    for (elem <- sorted) {
      println ("manual" + delim + elem._1 + delim + elem._2.uid + delim + elem._2.toStringSentWithUID())
    }

    if (notFound) println ("hadnotfound")

    println("")
  }

  def loadManualRatingsTSV(filename: String): Map[String, mutable.Map[String, Int]] = {
    val out = mutable.Map[String, mutable.Map[String, Int]]()
    var numRatingsLoaded:Int = 0

    println(" * Loading manual ratings: " + filename)
    var curQID:String = ""

    for (line <- io.Source.fromFile(filename, "UTF-8").getLines()) {
      val fields = line.split("\t")
      if (fields.length > 0) {
        if (fields(0).startsWith("QID")) {
          curQID = fields(1).trim
        } else if (fields(0).startsWith("manual")) {
          val uuid = fields(3).trim()
          val manualRating = fields(2).trim()

          if (manualRating.length > 0) {
            if (!out.contains(curQID)) {
              // If the key hasn't been used before, add a blank array
              out(curQID) = mutable.Map[String, Int]()
            }
            val UUIDtoRatingLUT = out(curQID)
            UUIDtoRatingLUT(uuid) = manualRating.toInt
            out(curQID) = UUIDtoRatingLUT

            if (manualRating.toInt != 1) {
              numRatingsLoaded += 1
            }
          }
        }
      }
    }

    println(" * Manual ratings loaded (number of keys: " + out.size + ", number of manual ratings: " + numRatingsLoaded + ")")

    // Return
    out.toMap
  }


  def summaryWithManualRatings(explQuestions: Array[MCExplQuestion], tablestore: TableStore, models: Array[Map[String, ArrayBuffer[String]]], manualRatings:Map[String, mutable.Map[String, Int]], topN:Int = 20): Unit = {
    // For each model
    for (modelIdx <- 0 until models.length) {
      val model = models(modelIdx)

      println ("Model " + modelIdx + ":  (topN = " + topN + ")")

      // For each question
      var numQuestions:Int = 0
      val hist = new Counter[String]
      for (question <- explQuestions) {
        val qid = question.question.questionID
        // Check to see if there are manual ratings for this question
        if (manualRatings.contains(qid)) {
          if (model.contains(qid)) {

            for (j <- 0 until topN) { // Examine top 20 rows
              val rankings = model(qid)
              val rowUUID = rankings(j)

              val manualRating = manualRatings(qid)(rowUUID)
              hist.incrementCount(manualRating.toString)
            }
            numQuestions += 1
          }
        }
      }

      // Display histogram
      println ("Histogram: (numQuestion = " + numQuestions + ")")
      var sum:Double = 0
      for (key <- hist.keySet) {
        sum += hist.getCount(key)
      }

      for (i <- 0 until 5) {
        val count = hist.getCount(i.toString)
        val prop = count.toDouble / sum

        println (i + ": \t" + count + "\t" + prop.formatted("%3.3f"))
      }

    }

  }


  /*
   * Scores
   */
  def calculateScores(questions:Array[MCExplQuestion], tablestore:TableStore, model:Map[String, ArrayBuffer[String]], textDesc:String = "", lexOverlapOnlyContentTags:Boolean = false): Unit = {
    var numSamples:Double = 0
    val errorsEncountered = new ArrayBuffer[Int]

    val questionScores = new ArrayBuffer[Counter[String]]

    for (i <- 0 until questions.length) {
      val question = questions(i)
      val explRowPool = new ExplRowPool(question, question.question.correctAnswer, tablestore)

      if (model.contains(question.question.questionID)) {
        val rankings = model(question.question.questionID)
        val scoresRanking = new ArrayBuffer[Double]
        for (i <- 0 until rankings.length) {
          explRowPool.addTablestoreRow( tablestore.getRowByUID(rankings(i)) )
          scoresRanking.append( -i )
        }

        explRowPool.populateExternalScores( scoresRanking.toArray )

        explRowPool.rank()

        /*
        println ("ExplRowPool: ")
        println (explRowPool.toString())
        println ("")
         */

        val scores = explRowPool.getScores(onlyContentTags = lexOverlapOnlyContentTags)
        //println ("Scores: " + scores)

        // Check for errors/infinities in the scores
        /*
        breakable {
          // Check for errors/infinities
          for (key <- scores.keySet) {
              val value = scores.getCount(key)
              if ((value == Double.NaN) || (value == Double.PositiveInfinity) || (value == Double.NegativeInfinity)) {
                scores.setCount(key, 0.0)
                //break()   // Found -- exit, without recording scores

                println("ERROR/INFINITY/NAN!")
                errorsEncountered.append(i)
              }
          }
        }
        */
        // Not found -- record scores
        questionScores.append( scores )

        numSamples += 1

      }

    }

    println ("Text Description: " + textDesc)
    println ("Summary:   (n = " + numSamples + ")")
    //println (summaryAverageScores(sumScores, numSamples))
    println ( ExplanationRegeneration.summaryAverageScores2(questionScores.toArray) )
    println ("lexOverlapOnlyContentTags: " + lexOverlapOnlyContentTags)

    println ("")
    println ("Errors encounterd with nan/invalid scores: " + errorsEncountered.length + " (" + errorsEncountered.mkString(", ") + ")")

  }


  /*
   * Old
   */


  // Performance by role overlap
  def performanceMAP(explQuestion: MCExplQuestion, tablestore: TableStore, models: Array[Map[String, ArrayBuffer[String]]], debugOutput:Boolean = false): Array[Double] = {

    val question = explQuestion.question
    val expl = explQuestion.expl

    val outAP = Array.fill[Double](models.length)(0.0)

    if (debugOutput) {
      println("Question: ")
      println(explQuestion.toString())
      println("")
    }


    for (i <- 0 until models.length) {
      if (debugOutput) println("Model " + i + " Rankings: ")

      val model = models(i)
      if (model.contains(question.questionID)) {
        val rankings = model(question.questionID)

        // Calculate MAP

        // Find ranks of gold
        var ranksOfGold = new ArrayBuffer[Int]
        for (a <- 0 until expl.length) {
          breakable {
            for (b <- 0 until rankings.length) {
              if (rankings(b) == expl(a).uid) {
                ranksOfGold.append(b + 1) // +1 to make it 1 indexed instead of 0 indexed
                break()
              }
            }
          }
        }

        ranksOfGold = ranksOfGold.sorted
        if (debugOutput) println("Ranks of Gold (normal MAP, no filtering) : " + ranksOfGold.mkString(", "))

        // Calculate MAP
        val ap = calculateAPFromRanks(ranksOfGold.toArray)
        if (debugOutput) println("Average Precision: " + ap.formatted("%3.3f"))
        outAP(i) = ap

        if (debugOutput) println("")

        // Display
        /*
        for (j <- 0 until math.min(rankings.length, 20)) {
          var isGold: Boolean = false
          breakable {
            for (a <- 0 until expl.length) {
              if (expl(a).uid.toLowerCase == rankings(j)) {
                isGold = true
                break()
              }
            }
          }

          if (isGold) {
            println(j + " : * " + rankings(j) + "\t" + tablestore.getRowByUID(rankings(j)).toStringText())
          } else {
            println(j + " :   " + rankings(j) + "\t" + tablestore.getRowByUID(rankings(j)).toStringText())
          }
        }
        */

      } else {
        println("ERROR: Question ID not found: " + question.questionID)
      }

      if (debugOutput) println("")

    }

    // Return average precisions of each model
    outAP
  }

  // Performance by role overlap
  def performanceByCategory(explQuestion: MCExplQuestion, tablestore: TableStore, roleToKeep: String = "CENTRAL", models: Array[Map[String, ArrayBuffer[String]]], debugOutput:Boolean = false): Array[Double] = {

    val question = explQuestion.question
    val expl = explQuestion.expl

    val outAP = Array.fill[Double](models.length)(0.0)

    if (debugOutput) {
      println("Question: ")
      println(explQuestion.toString())
      println("")
    }


    for (i <- 0 until models.length) {
      if (debugOutput) println("Model " + i + " Rankings: ")

      val model = models(i)
      if (model.contains(question.questionID)) {
        val rankings1 = model(question.questionID)

        // Copy
        val rankings = new ArrayBuffer[String]
        for (i <- 0 until rankings1.length) rankings.append(rankings1(i))

        // Remove rankings for gold rows that don't match the role
        for (a <- 0 until expl.length) {
          if (expl(a).role != roleToKeep) {
            breakable {
              for (b <- 0 until rankings.length) {
                if (rankings(b) == expl(a).uid) {
                  if (debugOutput) println("Removing " + rankings(b))
                  rankings.remove(b)
                  break()
                }
              }
            }
          }
        }

        // Calculate MAP

        // Find ranks of gold
        var ranksOfGold = new ArrayBuffer[Int]
        for (a <- 0 until expl.length) {
          if (expl(a).role == roleToKeep) {
            breakable {
              for (b <- 0 until rankings.length) {
                if (rankings(b) == expl(a).uid) {
                  ranksOfGold.append(b + 1) // +1 to make it 1 indexed instead of 0 indexed
                  break()
                }
              }
            }
          }
        }

        ranksOfGold = ranksOfGold.sorted
        if (debugOutput) println("Ranks of Gold (" + roleToKeep + " only) : " + ranksOfGold.mkString(", "))

        // Calculate MAP
        val ap = calculateAPFromRanks(ranksOfGold.toArray)
        if (debugOutput) println("Average Precision: " + ap.formatted("%3.3f"))
        outAP(i) = ap

        if (debugOutput) println("")

        // Display
        if (debugOutput) {
          for (j <- 0 until math.min(rankings.length, 20)) {
            var isGold: Boolean = false
            breakable {
              for (a <- 0 until expl.length) {
                if (expl(a).uid.toLowerCase == rankings(j)) {
                  isGold = true
                  break()
                }
              }
            }

            if (isGold) {
              println(j + " : * " + rankings(j) + "\t" + tablestore.getRowByUID(rankings(j)).toStringText())
            } else {
              println(j + " :   " + rankings(j) + "\t" + tablestore.getRowByUID(rankings(j)).toStringText())
            }
          }
        }
      } else {
        println("ERROR: Question ID not found: " + question.questionID)
      }

      if (debugOutput) println("")

    }

    // Return average precisions of each model
    outAP
  }


  // Performance by role overlap
  def performanceByLexicalOverlap(explQuestion: MCExplQuestion, tablestore: TableStore, overlap: Boolean = true, models: Array[Map[String, ArrayBuffer[String]]], onlyContentTags:Boolean = false, debugOutput:Boolean = false): Array[Double] = {

    val question = explQuestion.question
    val expl = explQuestion.expl

    val outAP = Array.fill[Double](models.length)(0.0)

    if (debugOutput) {
      println("Question: ")
      println(explQuestion.toString())
      println("")
    }

    val qWords = getQuestionAnswerWords(explQuestion, onlyContentTags)

    for (i <- 0 until models.length) {
      if (debugOutput) println("Model " + i + " Rankings: ")

      val model = models(i)
      if (model.contains(question.questionID)) {
        val rankings1 = model(question.questionID)

        // Copy
        val rankings = new ArrayBuffer[String]
        for (i <- 0 until rankings1.length) rankings.append(rankings1(i))

        // Remove rankings for gold rows that don't match the role
        var idx: Int = 0
        while (idx < rankings.length) {
          val row = tablestore.getRowByUID(rankings(idx))
          val rowHasOverlap = checkIfRowHasLexicalOverlap(row, qWords)
          if ((rowHasOverlap && overlap) || (!rowHasOverlap && !overlap)) {
            // Keep row
            idx += 1
          } else {
            // Remove row
            //println ("Removing " + rankings(idx))
            rankings.remove(idx)
          }
        }

        // Calculate MAP

        // Find ranks of gold
        var ranksOfGold = new ArrayBuffer[Int]
        for (a <- 0 until expl.length) {
          breakable {
            for (b <- 0 until rankings.length) {
              if (rankings(b) == expl(a).uid) {
                ranksOfGold.append(b + 1) // +1 to make it 1 indexed instead of 0 indexed
                break()
              }
            }
          }
        }

        ranksOfGold = ranksOfGold.sorted
        if (debugOutput) println("Ranks of Gold (overlap: '" + overlap + "' only) : " + ranksOfGold.mkString(", "))

        // Calculate MAP
        val ap = calculateAPFromRanks(ranksOfGold.toArray)
        if (debugOutput) println("Average Precision: " + ap.formatted("%3.3f"))
        outAP(i) = ap

        if (debugOutput) println("")

        // Display
        if (debugOutput) {
          for (j <- 0 until math.min(rankings.length, 20)) {
            var isGold: Boolean = false
            breakable {
              for (a <- 0 until expl.length) {
                if (expl(a).uid.toLowerCase == rankings(j)) {
                  isGold = true
                  break()
                }
              }
            }

            if (isGold) {
              println(j + " : * " + rankings(j) + "\t" + tablestore.getRowByUID(rankings(j)).toStringText())
            } else {
              println(j + " :   " + rankings(j) + "\t" + tablestore.getRowByUID(rankings(j)).toStringText())
            }
          }
        }
      } else {
        println("ERROR: Question ID not found: " + question.questionID)
      }

      if (debugOutput) println("")

    }

    // Return average precisions of each model
    outAP
  }


  /*
   * Supporting
   */
  def getQuestionAnswerWords(question: MCExplQuestion, onlyContentTags: Boolean = true): Set[String] = {
    // Step 1: Find lemmas in question and a given answer candidate
    val qWords = mutable.Set[String]()
    val answerCandidate = question.question.correctAnswer

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

    // Return
    qWords.toSet
  }


  def checkIfRowHasLexicalOverlap(row: TableRow, qWords: Set[String]): Boolean = {
    val words = row.getRowWordsStr()

    var hasOverlap: Boolean = false

    for (word <- words) {
      val lemma = LookupLemmatizer.getLemma(word)
      if ((qWords.contains(word)) || (qWords.contains(lemma))) {
        return true
      }
    }

    // Return
    false
  }


  // Calculate average precision from a set of ranks
  def calculateAPFromRanks(ranksIn: Array[Int]): Double = {
    var sumPrecisions: Double = 0.0
    val numRanks = ranksIn.length

    // Case: Empty ranks list
    if (numRanks == 0) return 0.0

    // Case: Non-empty ranks list
    for (i <- 0 until numRanks) {
      val numCorrectAtRank = i + 1
      val precision: Double = numCorrectAtRank.toDouble / ranksIn(i).toDouble
      sumPrecisions += precision
    }

    // Return average precision
    sumPrecisions / numRanks.toDouble
  }


  /*
   * Main entry point
   */
  def main(args: Array[String]) {
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
    var tablestoreIndex: String = ""
    if (props.getProperty("tablestoreIndex", "") != "") {
      tablestoreIndex = props.getProperty("tablestoreIndex", "")
    } else {
      throw new RuntimeException("ERROR: Unable to find 'tablestoreIndex' property in properties file.")
    }

    // Step 2B: Load the tablestore
    val tablestore = new TableStore(tablestoreIndex)

    //## Debug statements -- test usage of the tablestore
    println(tablestore.tables(tablestore.UIDtoTableLUT("a5c9-d7a4-8421-bb2e")).name)
    println(tablestore.getRowByUID("a5c9-d7a4-8421-bb2e"))


    // Step 3: Load the questions file that also includes the gold explanation graph data
    // Step 3A: Find question filename from the properties file
    var filenameQuestionsEval: String = ""

    if (props.getProperty("questionsEval", "") != "") {
      filenameQuestionsEval = props.getProperty("questionsEval", "")
    } else {
      throw new RuntimeException("ERROR: Unable to find 'questionsEval' property in properties file.")
    }


    // Step 3B: Load questions using question parser
    var questionsEval = ExamQuestionParserDynamic.loadQuestionsFromCSVList(filenameQuestionsEval, fullAnnotation = false, noAnnotation = false, tsvMode = true)

    // Step 3C: Convert questions from MCQuestions to a storage class that includes the explanation for the question, MCExplQuestion
    var explQuestionsEval = convertToExplQuestions(questionsEval)


    //## Debug: show that we've loaded the first 10 questions successfully
    println("Displaying first 10 questions (debug): ")
    for (i <- 0 until 10) {
      println(explQuestionsEval(i).toString)
      println("")
    }

    // Step 3D: Filter questions to only those that have been tagged as successfully annotated
    var filteredQuestionsEval = filterQuestionsByFlags(explQuestionsEval, Array("SUCCESS", "READY"))
    println("Loaded " + filteredQuestionsEval.size + " training questions after filtering. ")

    // If debugging, allow the user to limit the number of questions trained/evaluated for fast runtimes/iterations.
    val debugQuestionLimit = StringUtils.getInt(props, "debug.limitNumQuestions", 0)
    if (debugQuestionLimit > 0) {
      println("'debug.limitNumQuestions' property is enabled.  Limiting number of training and evaluation questions to " + debugQuestionLimit)
      //if (debugQuestionLimit > filteredQuestionsEval.length) throw new RuntimeException("ERROR: debug.limitNumQuestions (" + debugQuestionLimit + ") must be less than the number of training questions (" + filteredQuestionsTrain.length + ")")
      filteredQuestionsEval = filteredQuestionsEval.slice(0, debugQuestionLimit)
    }


    // Load model predictions
    val predictionsPath = "/home/peter/Downloads/tg2019-sharedtask-manualeval/"
    val predictionFilenames = Array("predict-tfidf.txt", "predict-jenlindadsouza.txt", "predict-pbannerj.txt", "predict-redkin.txt", "predict-ameyag416.txt")
    //val predictionFilenames = Array("predict-tfidf.txt")

    val models = new ArrayBuffer[Map[String, ArrayBuffer[String]]]
    for (filename <- predictionFilenames) {
      models.append(loadModelPredictions(predictionsPath + filename))
    }


    // Display predictions for questions
    /*
    for (question <- filteredQuestionsEval) {
      displayModelPredictions(question, tablestore, models.toArray)
    }
    */

/*
    val normalMAPs = Array.fill[Double](models.length)(0.0)
    var numSamples: Int = 0
    for (question <- filteredQuestionsEval) {
      val aps = performanceMAP(question, tablestore, models.toArray)
      for (j <- 0 until aps.length) {
        normalMAPs(j) += aps(j)
      }
      numSamples += 1
    }
    // Normalize to MAPs
    for (j <- 0 until normalMAPs.length) {
      normalMAPs(j) = normalMAPs(j) / numSamples
    }

    println("MAPS (normal, no filtering): ")
    for (j <- 0 until normalMAPs.length) {
      println("Model " + j + " (" + predictionFilenames(j) + "): " + normalMAPs(j).formatted("%3.3f"))
    }


    val roles = Array("CENTRAL", "GROUNDING", "LEXGLUE")
    for (role <- roles) {
      println("MAPS by role: " + role)

      var MAPs = Array.fill[Double](models.length)(0.0)
      var numSamples: Double = 0
      for (question <- filteredQuestionsEval) {
        val aps = performanceByCategory(question, tablestore, role, models.toArray)
        for (j <- 0 until aps.length) {
          MAPs(j) += aps(j)
        }
        numSamples += 1
      }

      // Normalize to MAPs
      for (j <- 0 until MAPs.length) {
        MAPs(j) = MAPs(j) / numSamples
      }
      println("\n\n")


      println("MAPS by role: " + role)
      for (j <- 0 until MAPs.length) {
        println("Model " + j + " (" + predictionFilenames(j) + "): " + MAPs(j).formatted("%3.3f"))
      }

      println("\n\n")
    }



    // Performance by lexical overlap

    val contentTagModes = Array(true, false)
    val overlapModes = Array(true, false)
    for (contentTagMode <- contentTagModes) {
      for (overlapMode <- overlapModes) {
        println("MAPS by lexical overlap mode: " + overlapMode + " contentTagMode: " + contentTagMode)

        var MAPs = Array.fill[Double](models.length)(0.0)
        var numSamples: Double = 0
        for (question <- filteredQuestionsEval) {
          val aps = performanceByLexicalOverlap(question, tablestore, overlapMode, models.toArray, onlyContentTags = contentTagMode)
          for (j <- 0 until aps.length) {
            MAPs(j) += aps(j)
          }
          numSamples += 1
        }

        // Normalize to MAPs
        for (j <- 0 until MAPs.length) {
          MAPs(j) = MAPs(j) / numSamples
        }
        println("\n\n")


        println("MAPS by lexical overlap mode: " + overlapMode + " contentTagMode: " + contentTagMode)
        for (j <- 0 until MAPs.length) {
          println("Model " + j + " (" + predictionFilenames(j) + "): " + MAPs(j).formatted("%3.3f"))
        }

        println("\n\n")
      }
    }

 */


    for (i <- 0 until models.length) {
      val model = models(i)
      println ("Model: " + predictionFilenames(i))

      calculateScores(filteredQuestionsEval, tablestore, model, predictionFilenames(i))
    }



/*
    println ("")

    for (question <- filteredQuestionsEval) {
      displayModelPredictionsSummaryTSV(question, tablestore, models.toArray)
    }

    val manualRatings = loadManualRatingsTSV("manual-expl-ratings.tsv")
    summaryWithManualRatings(filteredQuestionsEval, tablestore, models.toArray, manualRatings, topN = 20)
*/

  }


}
