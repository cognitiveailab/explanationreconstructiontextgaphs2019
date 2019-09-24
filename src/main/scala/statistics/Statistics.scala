package statistics

import java.io.PrintWriter
import java.util.Properties

import data.question.{ExamQuestionParserDynamic, MCExplQuestion}
import edu.arizona.sista.struct.Counter
import edu.arizona.sista.utils.StringUtils
import explanationgraph.TableStore
import explanationregeneration.ExplanationRegeneration.{calculateTableUseSummary, printUsage, _}

class Statistics {

}


object Statistics {
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
   * Additional statistics
   */
  def calculateExplanationLengthSummary(in:Array[MCExplQuestion], tablestore:TableStore): Counter[String] = {
    val hist = new Counter[String]()
    val maxLength = 16

    // For each question
    for (i <- 0 until in.size) {
      val explQuestion = in(i)
      val question = explQuestion.question
      val expl = in(i).expl

      if (expl.length <= maxLength) {
        hist.incrementCount("LENGTH_" + expl.length)
      } else {
        hist.incrementCount("LENGTH_" + maxLength)
      }

    }

    println ("Histogram of explanation lengths: ")
    for (i <- 0 to maxLength) {
      println ("LENGTH_" + i + " \t" + hist.getCount("LENGTH_" + i))
    }

    // Return
    hist
  }


  def calculateExplanationRoleSummary(in:Array[MCExplQuestion], tablestore:TableStore): Unit = {
    val hist = new Counter[String]()
    val maxLength = 16

    // Within question
    var sumCentral:Double = 0
    var sumGrounding:Double = 0
    var sumLexGlue:Double = 0
    var numCounts:Double = 0

    // Across question
    var countCentralAcross:Double = 0.0
    var countGroundingAcross:Double = 0.0
    var countLexGlueAcross:Double = 0.0

    // For each question
    for (i <- 0 until in.size) {
      val explQuestion = in(i)
      val question = explQuestion.question
      val expl = in(i).expl

      var countCentral:Double = 0
      var countGrounding:Double = 0
      var countLexGlue:Double = 0


      for (j <- 0 until expl.length) {
        println (expl(j).role)
        if (expl(j).role == "CENTRAL") {
          countCentral += 1
          countCentralAcross += 1
        }
        if (expl(j).role == "GROUNDING") {
          countGrounding += 1
          countGroundingAcross += 1
        }
        if (expl(j).role == "LEXGLUE") {
          countLexGlue += 1
          countLexGlueAcross += 1
        }
      }

      var sum = countCentral + countGrounding + countLexGlue
      var propCentral = countCentral / sum
      var propGrounding = countGrounding / sum
      var propLexGlue = countLexGlue / sum

      if (sum > 0) {
        sumCentral += propCentral
        sumGrounding += propGrounding
        sumLexGlue += propLexGlue
        numCounts += 1
      }
    }

    val pCentralOverall = sumCentral / numCounts
    val pGroundingOverall = sumGrounding / numCounts
    val pLexGlueOverall = sumLexGlue / numCounts


    println ("Proportion of explanatory roles (within question):")
    println ("CENTRAL: \t" + pCentralOverall.formatted("%3.3f"))
    println ("GROUNDING: \t" + pGroundingOverall.formatted("%3.3f"))
    println ("LEXGLUE: \t" + pLexGlueOverall.formatted("%3.3f"))
    println ("")

    var sumAcross = countCentralAcross + countGroundingAcross + countLexGlueAcross
    val pCentralOverallAcross = countCentralAcross / sumAcross
    val pGroundingOverallAcross = countGroundingAcross / sumAcross
    val pLexGlueOverallAcross = countLexGlueAcross / sumAcross

    println ("Proportion of explanatory roles (across questions):")
    println ("CENTRAL: \t" + pCentralOverallAcross.formatted("%3.3f"))
    println ("GROUNDING: \t" + pGroundingOverallAcross.formatted("%3.3f"))
    println ("LEXGLUE: \t" + pLexGlueOverallAcross.formatted("%3.3f"))

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

    // Step 3C: Convert questions from MCQuestions to a storage class that includes the explanation for the question, MCExplQuestion
    var explQuestionsTrain = convertToExplQuestions(questionsTrain)


    //## Debug: show that we've loaded the first 10 questions successfully
    println ("Displaying first 10 questions (debug): ")
    for (i <- 0 until 10) {
      println (explQuestionsTrain(i).toString)
      println ("")
    }

    // Step 3D: Filter questions to only those that have been tagged as successfully annotated
    var filteredQuestionsTrain = filterQuestionsByFlags(explQuestionsTrain, Array("SUCCESS", "READY") )
    println ("Loaded " + filteredQuestionsTrain.size + " training questions after filtering. ")

    // If debugging, allow the user to limit the number of questions trained/evaluated for fast runtimes/iterations.
    val debugQuestionLimit = StringUtils.getInt(props, "debug.limitNumQuestions", 0)
    if (debugQuestionLimit > 0) {
      println ("'debug.limitNumQuestions' property is enabled.  Limiting number of training and evaluation questions to " + debugQuestionLimit)
      if (debugQuestionLimit > filteredQuestionsTrain.length) throw new RuntimeException("ERROR: debug.limitNumQuestions (" + debugQuestionLimit + ") must be less than the number of training questions (" + filteredQuestionsTrain.length + ")")
      filteredQuestionsTrain = filteredQuestionsTrain.slice(0, debugQuestionLimit)
    }


    // Step 4: Compute The proportion of explanations that contain knowledge from a given table (Table 3 in LREC 2018 paper)
    // Note: This is not important to this experiment, and is just an informative repeat of one of the analyses in the LREC2018 paper.
    calculateTableUseSummary(filteredQuestionsTrain, tablestore)
    println ("\n\n")


    calculateExplanationLengthSummary(filteredQuestionsTrain, tablestore)
    println ("\n\n")

    calculateExplanationRoleSummary(filteredQuestionsTrain, tablestore)
    println ("\n\n")

  }

}