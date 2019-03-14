package bootstrap

import data.question.{ExamQuestionParserDynamic, MCQuestion}
import questionclassification.ensemble.SummaryModelPerformance.loadBERTQAPerformanceNative

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Performs bootstrap resampling between a number of QC runs.
  * Created by peter on 3/2/19.
  */
class StatsBootstrapQC {

}

object StatsBootstrapQC {

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


  // Load raw scores from a .stats. output file from the QC system (QuestionTopicClassifier)
  // Format is: qid \t map \t at1 \t hard
  def loadQCStatsFile(filename:String, whichStatistic:String = ""):Map[String, Double] = {
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
      if (fields.length != 4) throw new RuntimeException("ERROR: BERT Predictions file contains more or less than 4 columns (" + filename + ")")

      val qid = fields(0)
      val map = fields(1).toDouble
      val at1 = fields(2).toDouble

      // Store correctness in map (key is question ID)
      if (whichStatistic == "MAP") {
        out += (qid -> map)
      } else if (whichStatistic == "AT1") {
        out += (qid -> at1)
      } else {
        throw new RuntimeException("ERROR: whichStatistic not recognized (" + whichStatistic + ")")
      }
    }

    // return
    out.toMap
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
    // Bootstrap resampling
    val numSamples = 10000
    val whichStatistic = "MAP"
    //val whichStatistic = "AT1"


    // Loading BERT QA performance from raw files (slightly more involved)
    val filenameQuestionsDevVikasOrder = "annotation/question-topic-classifier/vikas_large_models/Vikas-ARC-All-Dev.tsv"
    val questionsDevVikasOrder = ExamQuestionParserDynamic.loadQuestionsFromCSVList(filenameQuestionsDevVikasOrder, fullAnnotation = false, noAnnotation = false, annotateExplanations = false, tsvMode = true)
    val filenameQuestionsTestVikasOrder = "annotation/question-topic-classifier/vikas_large_models/Vikas-ARC-All-Test.tsv"
    val questionsTestVikasOrder = ExamQuestionParserDynamic.loadQuestionsFromCSVList(filenameQuestionsTestVikasOrder, fullAnnotation = false, noAnnotation = false, annotateExplanations = false, tsvMode = true)


    val filenameBaseline = "/data/questionclassificationdata_BATCHARC3/qc-TT-CLSVM-TRQ3370-TSTQ3548-FEATURES-UNI_TG_Q-UNI_Q-UNI_TG_A-BI_Q-BI_TG_A-UNI_A-HIERARCHALL-BI_TG_Q-BI_A-S1E6-W2V-GLOVE.6B.50D.TXT-TEST.L6.stats.txt"

    // Essential Terms
    //val filenameExp = "/data/questionclassificationdata_BATCHARC3/qc-TT-CLSVM-TRQ3370-TSTQ3548-FEATURES-UNI_TG_Q-ESSENTIAL_Q-UNI_Q-UNI_TG_A-BI_Q-BI_TG_A-UNI_A-HIERARCHALL-BI_TG_Q-BI_A-S1E6-W2V-GLOVE.6B.50D.TXT-TEST.L6.stats.txt"

    // Dependencies
    //val filenameExp = "/data/questionclassificationdata_BATCHARC3/qc-TT-CLSVM-TRQ3370-TSTQ3548-FEATURES-UNI_TG_Q-UNI_Q-UNI_TG_A-BI_Q-BI_TG_A-UNI_A-HIERARCHALL-BI_TG_Q-DEP-BI_A-S1E6-W2V-GLOVE.6B.50D.TXT-TEST.L6.stats.txt"

    // WN Expansion
    //val filenameExp = "/data/questionclassificationdata_BATCHARC3/qc-TT-CLSVM-TRQ3370-TSTQ3548-FEATURES-UNI_TG_Q-UNI_Q-WNEXPQDEP-UNI_TG_A-BI_Q-BI_TG_A-UNI_A-HIERARCHALL-BI_TG_Q-BI_A-S1E6-W2V-GLOVE.6B.50D.TXT-TEST.L6.stats.txt"

    // BERT-QC
    // Note: this .stats. file was generated using the ConvertBertQCOutputToQCScores.scala tool (questionclassification.bert package)
    val filenameExp = "/home/peter/Downloads/Bert-Base-Classdisst/test/BERT-Base-TEST.L" + 6 + ".classdist.txt.stats.txt"



    val scoresBaseline_ = loadQCStatsFile(filenameBaseline, whichStatistic)
    val scoresExp_ = loadQCStatsFile(filenameExp, whichStatistic)

    val scoresBaseline = convertScoresMapToArray(scoresBaseline_, questionsTestVikasOrder)
    val scoresExp = convertScoresMapToArray(scoresExp_, questionsTestVikasOrder)

    val p = BootstrapResampling.computeBootstrapResampling(scoresBaseline, scoresExp, numSamples = numSamples)

    // Summary output

    println ("")
    println ("filenameBaseline: " + filenameBaseline)
    println ("filenameExp: " + filenameExp)
    println ("numBootstrapSamples: " + numSamples)
    println ("whichStatistic: " + whichStatistic)
    println ("p = " + p)
  }

}


