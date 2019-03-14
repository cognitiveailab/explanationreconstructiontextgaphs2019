package explanationregeneration

import java.io.PrintWriter

import data.question.{MCExplQuestion, MCQuestion}
import edu.arizona.sista.struct.Counter
import explanationgraph.{TableRow, TableStore}
import util.CounterIO

import scala.collection.mutable.ArrayBuffer


/**
  * A set of classes that store how often a given table row tends to occur with questions of a specific question classification label.
  * For example, the table row "boiling means changing from a liquid to a gas by adding heat energy" likely occurs most often
  * with questions having the MAT_COS_BOILING question classification label than other labels.
  *
  * This set of classes stores these distributions, and allows querying them at multiple levels of trunctation
  * (e.g. probabilities for just MAT, or MAT_COS, instead of the full MAT_COS_BOILING question classification label).
  *
  * These distributions are fairly quick to compute for one question, but can take a while when aggregated across
  * thousands of questions.  Some facilities are provided to save these QC distributions to files (a cache), so that
  * this cache can be loaded up later to decrease runtime.  Note that the cache can be sizeable (a few gb).
  *
  * Created by user on 4/10/18.
  */

// NOTE: These should rarely be created on their own, but instantiated from the mkQCExplanationRowPredictor() generator (or, loaded from a file)
// Note: heirLevel = -1 disables QC label truncation, and uses full labels.
class QCExplanationRowPredictor(val rowDists:ArrayBuffer[QCRowDist], var qcFreq:Counter[String], var roleFreq:Counter[String], tablestore:TableStore, heirLevel:Int) {
  // Make a blank, untrained QCExplanationRowPredictor
  // Note: train() must be called after creating this, or it will be empty.
  def this(tablestore:TableStore, heirLevel:Int = -1) {
    this(new ArrayBuffer[QCRowDist], new Counter[String], new Counter[String], tablestore, heirLevel)
  }


  /*
   * Loading/Saving
   */
  def saveToString():String = {
    val os = new StringBuilder
    val lineDelim = "\r\n"

    // Save heirLevel
    os.append(heirLevel + lineDelim)

    // Save Row Dists
    os.append(rowDists.length + lineDelim)
    for (i <- 0 until rowDists.length) {
      os.append( rowDists(i).saveToString() + lineDelim )
    }

    // Save Frequencies
    os.append( CounterIO.saveCounterToString(qcFreq) + lineDelim)
    os.append( CounterIO.saveCounterToString(roleFreq) + lineDelim)

    // Return
    os.toString()
  }


  /*
   * ...
   */

  // Make a frequency distribution of the question classes observed in an array of questions
  def mkQCFreqDist(questions:Array[MCExplQuestion]):Counter[String] = {
    val out = new Counter[String]

    for (question <- questions) {
      val qcLabels = question.question.topic
      for (qcLabel <- qcLabels) {
        out.incrementCount( mkHeirarchicalLabel(qcLabel), 1.0)
      }
    }

    // Return
    out
  }


  // Make a frequency distribution of the explanation roles observed in an array of explanations
  def mkRoleFreqDist(questions:Array[MCExplQuestion]):Counter[String] = {
    val out = new Counter[String]

    for (question <- questions) {
      val expls = question.expl
      for (expl <- expls) {
        val roleLabel = expl.role
        out.incrementCount(roleLabel)
      }
    }

    //return
    out
  }


  /*
   * Training
   */
  // Calculate the frequency distribution of QC classes for each row
  def train(questionsIn:Array[MCExplQuestion]): Unit = {
    qcFreq = mkQCFreqDist(questionsIn)
    roleFreq = mkRoleFreqDist(questionsIn)

    // Step 1: For each row, calculate frequency distributions for question classes and question roles
    for (explQuestion <- questionsIn) {
      val question = explQuestion.question
      val qcLabels = question.topic
      val expl = explQuestion.expl

      for (explRow <- expl) {
        val uid = explRow.uid
        val role = explRow.role
        val rowDist = findRowDist(uid)

        // Role
        rowDist.roleDistFreq.incrementCount(role)

        // Categories
        for (qcLabel <- qcLabels) {
          rowDist.QCDistFreq.incrementCount( mkHeirarchicalLabel(qcLabel) )
          // temporarily removing this
          //val maxLevel = category.count(_ == '_')
          //println ("Category: " + category + "  maxLevel: " + maxLevel)
          //for (i <- 1 to maxLevel) {
          //  val label = getHierarchicalLabels(category, i)
          // rowDist.QCDistFreq.incrementCount(label)
          //}
        }
      }
    }


    // Step 2: Calculate probability distributions from frequency distributions
    //for row distribution in the set
    // should we add the role prob dist here as well?
    for (rowDist <- rowDists) {
      for (qcLabel <- rowDist.QCDistFreq.keySet) {
        val truncatedQCLabel = mkHeirarchicalLabel(qcLabel)
        val count = rowDist.QCDistFreq.getCount(truncatedQCLabel)        // Total number of times we've observed this row paired with this QC label
        val sum = qcFreq.getCount(truncatedQCLabel)                      // Total number of questions that we've observed this QC label
        val prob = count / sum
        rowDist.QCDistProb.setCount(truncatedQCLabel, prob)
      }


      // TODO: Normalize by number of occurrences of row (i.e. row frequency)
      /*
      for (i <- 0 until rowRoles.length) {
        rowDist.roleDistProb.setCount(rowRoles(i), counts(i) / roleFreq.getCount(rowRoles(i)))
      }
      */

      //println ("QCProbDist: " + rowDist.QCDistProb.sorted(descending = true))

    }

  }


  /*
   * Query/use
   */
  // Input: Question category
  // Output: List of most likely rows to belong to that category
  def queryCategory(qcLabel:String):Array[RowScore] = {
    val out = new ArrayBuffer[RowScore]

    for (i <- 0 until rowDists.length) {
      val rowProb = rowDists(i).QCDistProb.getCount( mkHeirarchicalLabel(qcLabel) )

      // Save row/score evaluation
      out.append( new RowScore(rowDists(i), rowProb) )
    }

    // Sort by scores in descending order
    val sorted = out.sortBy(- _.score).toArray

    // Return
    sorted
  }


  def findRowDist(uid:String):QCRowDist = {
    for (i <- 0 until rowDists.length) {
      if (rowDists(i).row.uid == uid) return rowDists(i)
    }

    // Not found -- create new rowDist
    val qcDist = new QCRowDist(tablestore.getRowByUID(uid), new Counter[String], new Counter[String], new Counter[String], new Counter[String])
    rowDists.append(qcDist)

    // Return
    qcDist
  }


  /*
   * Supporting functions
   */


  def display(maxDisplay:Int = -1) {
    val sorted = rowDists.sortBy(- _.numOccurances)
    var numToDisplay:Int = sorted.length
    if (maxDisplay > 0) {
      if (maxDisplay > sorted.length) {
        numToDisplay = sorted.length
      } else {
        numToDisplay = maxDisplay
      }
    }

    for (i <- 0 until numToDisplay) {
      println (i + "\t" + sorted(i))
    }
  }


  // Wrapper for safely calling getHierarchicalLabel, while checking for the edge case that truncated levels may be disabled.
  def mkHeirarchicalLabel(label:String):String = {
    // Case 1: heirarchical level is set to 0 or -1, signifying that it's disabled, and we should just return the full label
    if (heirLevel <= 0) return label

    // Case 2: hierarchical level is populated and valid, so we should return a truncated label
    val truncatedLabel = getHierarchicalLabels(label, heirLevel)
    // Return
    truncatedLabel
  }

  // Convert a full QC label to one truncated to a certain level of the QC hierarchy
  def getHierarchicalLabels(label:String, level:Int):String = {
    var new_label = ""
    val labels = label.split('_')
    if(labels.length >= level) {
      val new_labels = labels.take(level)
      new_label = new_labels.mkString("_")
    } else {
      new_label = label
    }
    new_label
  }


}



object QCExplanationRowPredictor {

  // Generator
  def mkQCExplanationRowPredictor(questionsTrain:Array[MCExplQuestion], tablestore:TableStore, heirLevel:Int = -1):QCExplanationRowPredictor = {
    val out = new QCExplanationRowPredictor(tablestore, heirLevel)
    out.train(questionsTrain)
    // Return
    out
  }

  // Loading
  def loadFromString(strIn:String, tablestore:TableStore) = {
    val lineDelim = "\r\n"
    val elements = strIn.split(lineDelim, -1)   // return trailing empty strings
    var atElem:Int = 0

    // load heirLevel
    val heirLevel = elements(atElem).toInt
    atElem += 1

    // Read number of rowDists to load
    val numRowDists = elements(atElem).toInt
    atElem += 1

    val rowDists_ = new ArrayBuffer[QCRowDist]
    for (i <- 0 until numRowDists) {
      rowDists_.append( QCRowDist.loadFromString( elements(atElem), tablestore) )
      atElem += 1
    }

    // Read frequencies
    val qcFreq_ = CounterIO.loadCounterFromString(elements(atElem))
    atElem += 1

    val roleFreq_ = CounterIO.loadCounterFromString(elements(atElem))
    atElem += 1


    // Return new object instantiated with the data we've just loaded
    new QCExplanationRowPredictor(rowDists_, qcFreq_, roleFreq_, tablestore, heirLevel)
  }



}



// Storage class for a group of QCExplanationRowPredictors, from level 0 (full) to maxLevel
// NOTE: This should rarely be called, and instead instantiated with the mkQCRowProbGroup generator, or loaded from a file.
class QCRowProbGroup(val levels:Array[QCExplanationRowPredictor], tablestore:TableStore) {
  //val levels = new Array[QCExplanationRowPredictor](maxLevel+1)
  val maxLevel:Int = levels.length-1

  // Alternate constructor that generates a blank QCRowProbGroup -- generateGroups() must be called after this.
  def this(tablestore:TableStore, maxLevel:Int) {
    this(new Array[QCExplanationRowPredictor](maxLevel+1), tablestore)
  }


  /*
   * Initialization
   */
  // Generate QCExplanationRowPredictor for each level
  def generateGroups(questionsTrain:Array[MCExplQuestion]) {
    for (i <- 0 to maxLevel) {
      levels(i) = QCExplanationRowPredictor.mkQCExplanationRowPredictor(questionsTrain, tablestore, i)
    }
  }

  /*
   * Accessors/queries
   */

  // query the QC category labels at a specific level
  def queryCategoryAtLevel(qcLabel:String, level:Int):Array[RowScore] = {
    levels(level).queryCategory(qcLabel)
  }

  def getAtLevel(level:Int):QCExplanationRowPredictor = {
    levels(level)
  }

  /*
   * Save to file
   */
  def saveToFiles(filenamePrefix:String) {
    for (i <- 0 until maxLevel+1) {
      val filename = filenamePrefix + "-L" + i + ".qcrpg"
      val pw = new PrintWriter(filename)
      pw.println( levels(i).saveToString() )
      pw.close()
    }
  }

}


object QCRowProbGroup {

  // Generator
  def mkQCRowProbGroup(questionsTrain:Array[MCExplQuestion], tablestore:TableStore, maxLevel:Int):QCRowProbGroup = {
    val out = new QCRowProbGroup(tablestore, maxLevel)
    out.generateGroups(questionsTrain)
    // Return
    out
  }


  // Loading from files
  def loadFromFiles(filenamePrefix:String, tablestore:TableStore, maxLevel:Int):QCRowProbGroup = {
    val out = new QCRowProbGroup(tablestore, maxLevel)
    for (i <- 0 until maxLevel + 1) {
      val filename = filenamePrefix + "-L" + i + ".qcrpg"
      val str = io.Source.fromFile(filename, "UTF-8").mkString
      out.levels(i) = QCExplanationRowPredictor.loadFromString(str, tablestore)
    }
    // Return
    out
  }

  // Check to see if a set of files already exists to load this QCRowProbGroup
  def canLoadFromFiles(filenamePrefix:String, tablestore:TableStore, maxLevel:Int):Boolean = {
    val out = new QCRowProbGroup(tablestore, maxLevel)
    for (i <- 0 until maxLevel + 1) {
      val filename = filenamePrefix + "-L" + i + ".qcrpg"
      if (!(new java.io.File(filename).exists())) {
        // At least one of the files not found -- return false
        return false
      }
    }
    // If we reach here, all the files for this set exist
    true
  }


}


// Storage class
class RowScore(val rowDist:QCRowDist, val score:Double) {

}




// Storage class
class QCRowDist(val row:TableRow, val QCDistFreq:Counter[String], val QCDistProb:Counter[String],
                                  val roleDistFreq:Counter[String], val roleDistProb:Counter[String]) {


  // Count the total number of explanations that this row has appeared in
  def numOccurances:Double = {
    var sum:Double = 0.0
    for (key <- roleDistFreq.keySet) {
      sum += roleDistFreq.getCount(key)
    }
    // Return
    sum
  }


  /*
   * Save to string
   */
  def saveToString():String = {
    val os = new StringBuilder
    val lineDelim = "\n"

    // Save row UUID
    os.append(row.uid + lineDelim)

    // Frequencies
    os.append(CounterIO.saveCounterToString(QCDistFreq) + lineDelim)
    os.append(CounterIO.saveCounterToString(QCDistProb) + lineDelim)
    os.append(CounterIO.saveCounterToString(roleDistFreq) + lineDelim)
    os.append(CounterIO.saveCounterToString(roleDistProb) + lineDelim)

    // Return
    os.toString()
  }


  /*
   * String
   */

  override def toString():String = {
    val os = new StringBuilder
    val roleDistSorted = roleDistFreq.sorted(descending = true)
    val roleProbSorted = roleDistProb.sorted(descending = true)
    val QCDistSorted = QCDistFreq.sorted(descending = true)
    val QCProbSorted = QCDistProb.sorted(descending = true)

    os.append("QCRowDist: " + row.toStringSentWithUID() + "\n")
    os.append("   roleDistFreq: ")
    for (kvp <- roleDistSorted) {
      os.append(kvp._1 + ":" + kvp._2.formatted("%3.3f") + "  ")
    }
    os.append("\n")

    os.append("   roleDistProb: ")
    for (kvp <- roleProbSorted) {
      os.append(kvp._1 + ":" + kvp._2.formatted("%3.3f") + "  ")
    }
    os.append("\n")

    os.append("   QCDistFreq: ")
    for (kvp <- QCDistSorted) {
      os.append(kvp._1 + ":" + kvp._2.formatted("%3.3f") + "  ")
    }
    os.append("\n")

    os.append("   QCDistProb: ")
    for (kvp <- QCProbSorted) {
      os.append(kvp._1 + ":" + kvp._2.formatted("%3.3f") + "  ")
    }
    os.append("\n")

    // Return
    os.toString()
  }
}


object QCRowDist {

  // Load from a string
  def loadFromString(strIn:String, tablestore:TableStore):QCRowDist = {
    val lineDelim = "\n"
    val elements = strIn.split(lineDelim, -1)   // return trailing empty strings

    val uid = elements(0)
    val row = tablestore.getRowByUID(uid)
    val QCDistFreq = CounterIO.loadCounterFromString(elements(1))
    val QCDistProb = CounterIO.loadCounterFromString(elements(2))
    val roleDistFreq = CounterIO.loadCounterFromString(elements(3))
    val roleDistProb = CounterIO.loadCounterFromString(elements(4))

    // Create
    new QCRowDist(row = row, QCDistFreq = QCDistFreq, QCDistProb = QCDistProb,
                              roleDistFreq = roleDistFreq, roleDistProb = roleDistProb)
  }

}