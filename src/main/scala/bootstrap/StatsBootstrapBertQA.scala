package bootstrap

import data.question.{ExamQuestionParserDynamic, MCQuestion}
import questionclassification.ensemble.SummaryModelPerformance.loadBERTQAPerformanceNative

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Performs bootstrap resampling between a number of BERT Multiple Choice QA runs.
  * Created by peter on 3/2/19.
  */
class StatsBootstrapBertQA {

}


object StatsBootstrapBertQA {

  // Load selected answers from BERT QC model (TSV output from the native BERT classifier)
  // Note: Assumes that the output is in the same order as the question file
  def readBERTQAPerformanceNative(filename:String, questions:Array[MCQuestion]):(Map[String, Double], Double) = {
    val out = mutable.Map[String, Double]()

    var atQuestionIdx:Int = 0
    var atAnswerIdx:Int = 0
    val curQScores = new ArrayBuffer[Double]

    var numCorrect:Double = 0

    //println ("questions.size: " + questions.length)

    for (line <- io.Source.fromFile(filename, "UTF-8").getLines()) {

      //println ("line: " + line)
      // Step 1: Read scores for this line
      val fields = line.split("\t").map(_.trim())
      // Assumption is that there are two columns, 0 (incorrect) and 1 (correct).
      if (fields.length != 2) throw new RuntimeException("ERROR: BERT Predictions file contains more than two columns (" + filename + ")")
      val probAnswerIdxCorrect = fields(1).toDouble
      curQScores.append(probAnswerIdxCorrect)

      // Step 2: Handle question incrementing logic
      atAnswerIdx += 1

      val curQuestion = questions(atQuestionIdx)
      if (atAnswerIdx >= curQuestion.choices.length) {
        // Store current results
        var isCorrect:Double = 0.0
        val scores = curQScores.zipWithIndex.sortBy(-_._1)
        if (scores(0)._2 == curQuestion.correctAnswer) {
          // Correct
          isCorrect = 1.0
          numCorrect += 1
        }

        // Store correctness in map (key is question ID)
        out += (curQuestion.questionID -> isCorrect)
        //## Debug
        /*
        println ("curQScores: " + curQScores.mkString(", ") +  "  qIdx: " + atQuestionIdx)

        if (curQuestion.choices.length != 4) {
          println (" CHOICES: " + curQuestion.choices.length)
          println ("question: " + curQuestion.toString)
        }
        */

        // Reset for next question
        atAnswerIdx = 0
        atQuestionIdx += 1
        curQScores.clear()
      }

    }

    val accuracy = numCorrect / out.keySet.toArray.size.toDouble
    println ("* Accuracy of QA Performance from loaded native BERT file is " + accuracy.formatted("%3.4f") + " ( " + filename + " )")

    // return
    (out.toMap, accuracy)
  }


  // Converts from a map of (questionid, score) to an array of scores in a specific order specified by the order of 'questions'
  def convertScoresMapToArray(scores:Map[String, Double], questions:Array[MCQuestion]):Array[Double] = {
    val out = new ArrayBuffer[Double]
    for (question <- questions) {
      out.append( scores(question.questionID) )
    }
    // Return
    out.toArray
  }


  def main(args:Array[String]): Unit = {
    // Loading BERT QA performance from raw files (slightly more involved)
    val filenameQuestionsDevVikasOrder = "annotation/question-topic-classifier/vikas_large_models/Vikas-ARC-All-Dev.tsv"
    val questionsDevVikasOrder = ExamQuestionParserDynamic.loadQuestionsFromCSVList(filenameQuestionsDevVikasOrder, fullAnnotation = false, noAnnotation = false, annotateExplanations = false, tsvMode = true)
    val filenameQuestionsTestVikasOrder = "annotation/question-topic-classifier/vikas_large_models/Vikas-ARC-All-Test.tsv"
    val questionsTestVikasOrder = ExamQuestionParserDynamic.loadQuestionsFromCSVList(filenameQuestionsTestVikasOrder, fullAnnotation = false, noAnnotation = false, annotateExplanations = false, tsvMode = true)

    val filenamesNoLabelsDev = new ArrayBuffer[String]
    val filenamesNoLabelsTest = new ArrayBuffer[String]
    val numRuns = 10
    val numTruncationLevels = 6

    for (runIdx <- 1 to numRuns) {
      filenamesNoLabelsDev.append("annotation/question-topic-classifier/vikas_large_models/Base_model/dev/dev_results_" + runIdx + ".tsv")
      filenamesNoLabelsTest.append("annotation/question-topic-classifier/vikas_large_models/Base_model/test/test_results_" + runIdx + ".tsv")
    }


    val filenamesLabelsDev = new ArrayBuffer[Array[String]]
    val filenamesLabelsTest = new ArrayBuffer[Array[String]]
    for (truncationLevel <- 1 to numTruncationLevels) {
      val fdev = new ArrayBuffer[String]
      val ftest = new ArrayBuffer[String]
      for (runIdx <- 1 to numRuns) {
        fdev.append("annotation/question-topic-classifier/vikas_large_models/Exp_3a/dev_files_" + truncationLevel + "/test_results_" + runIdx + ".tsv")
        ftest.append("annotation/question-topic-classifier/vikas_large_models/Exp_3a/test_files_" + truncationLevel + "/test_results_" + runIdx + ".tsv")
      }
      filenamesLabelsDev.append(fdev.toArray)
      filenamesLabelsTest.append(ftest.toArray)
    }

    // Bootstrap resampling
    val numSamples = 10000


    //##
    // Do pairwise comparisons of each run, get p value
    for (truncationLevel <- 5 until 6) {
      println("Level " + (truncationLevel + 1))
      val pValues = new ArrayBuffer[Double]

      //for (i <- 0 until numRuns) {
      for (j <- 0 until numRuns) {
        // Load results
        //val (scoresBaseline_, accuracyBaseline) = readBERTQAPerformanceNative(filenamesNoLabelsTest(i), questionsTestVikasOrder)
        val (scoresBaseline_, accuracyBaseline) = readBERTQAPerformanceNative(filenamesNoLabelsTest(j), questionsTestVikasOrder)
        val (scoresExp_, accuracyExp) = readBERTQAPerformanceNative(filenamesLabelsTest(truncationLevel)(j), questionsTestVikasOrder)
        // Convert from scala maps to arrays
        val scoresBaseline = convertScoresMapToArray(scoresBaseline_, questionsTestVikasOrder)
        val scoresExp = convertScoresMapToArray(scoresExp_, questionsTestVikasOrder)
        // Perform bootstrap resampling on this pairwise comparison, store p value

        println("delta: " + (accuracyExp - accuracyBaseline))

        val p = BootstrapResampling.computeBootstrapResampling(scoresBaseline, scoresExp, numSamples = numSamples)
        pValues.append(p)
      }
      //}

      println ("P Values: ")
      for (p <- pValues) {
        println(p)
      }
    }


  }

}