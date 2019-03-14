package data.essentialterms

import edu.arizona.sista.struct.Counter

/**
  * Load essential terms from a pre-computed essential term file in the format of Daniel Khashabi
  * Created by peter on 1/24/19.
  */

class EssentialTerms(filenames:Array[String], binary:Boolean = false) {
  val QIDtoTermLUT = scala.collection.mutable.Map[String, Counter[String]]()
  var notFoundQueries:Int = 0

  // Constructor -- load essential terms from file
  for (filename <- filenames) {
    loadEssentialTermsFromFile(filename, binary)
  }


  // Loading essential terms
  def loadEssentialTermsFromFile(filename:String, binary:Boolean, lowercase:Boolean = true): Unit = {
    println (" * Loading Essential Terms from file (" + filename + ")")

    for (line <- io.Source.fromFile(filename, "UTF-8").getLines()) {
      val fields = line.split("\t")
      if (fields.length > 1) {
        val termWeights = new Counter[String]

        // Get question ID
        val qId = fields(0)
        val question = fields(1)
        val termsBinary = fields(2)
        val termsWeights = fields(3).substring(4, fields(3).length-1)     // Remove "Map(" ... ")"


        if (binary == true) {
          // Load binary values (a word is either essential or not)
          val split = termsBinary.trim().split("//")
          for (term <- split) {
            if (lowercase == true) {
              termWeights.setCount(term.toLowerCase, 1.0)
            } else {
              termWeights.setCount(term, 1.0)
            }
          }

        } else {
          // Load weights

          // Parse each term-value pair
          val termValuePairs = termsWeights.split(",")
          for (tvp <- termValuePairs) {
            println(tvp)
            val split = tvp.trim().split("->")
            if (split.length == 2) {
              val term = split(0).trim()
              val weight = split(1).trim().toDouble
              if (weight > 0) {
                if (lowercase == true) {
                  termWeights.setCount(term, weight)
                } else {
                  termWeights.setCount(term.toLowerCase, weight)
                }
              }
            }
          }
        }

        QIDtoTermLUT(qId) = termWeights
      }
    }
  }


  // Accessors
  def getEssentialTerms(qId:String):Counter[String] = {
    if (QIDtoTermLUT.contains(qId)) {
      return QIDtoTermLUT(qId)
    }

    // Default return -- QID not found
    println ("WARNING: getEssentialTerms(): QID not found (" + qId + ") ")
    notFoundQueries += 1

    /*
    if (notFoundQueries > 3) {
      throw new RuntimeException("ERROR: getEssentialTerms(): number of not found queries has exceeded threshold (3). Exiting. ")
    }
    */
    return new Counter[String]
  }


  // String
  override def toString():String = {
    val os = new StringBuilder

    for (key <- QIDtoTermLUT.keySet) {
      os.append( getEssentialTerms(key).sorted(descending = true) + "\n")
    }

    os.toString()
  }
}



// Load Test
object EssentialTerms {

  def main(args: Array[String]): Unit = {
    val filenames = Array("annotation/question-topic-classifier/ARC-Train-ET.tsv", "annotation/question-topic-classifier/ARC-Dev-ET.tsv", "annotation/question-topic-classifier/ARC-Test-ET.tsv")
    val essentialTerms = new EssentialTerms(filenames)

    println (essentialTerms.toString())
  }

}

