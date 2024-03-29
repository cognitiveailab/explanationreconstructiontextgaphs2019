package explanationgraph

import org.slf4j.LoggerFactory

import scala.collection.mutable

/**
  * A "look-up table lemmatizer", based on hard-coded word -> lemma pairs.
  * This is used for platforms where there aren't easily available lemmatizers (like node.js, the platform the WorldTree annotation tool used).
  * Created by peter on 9/13/17.
  */
class LookupLemmatizer {

}

object LookupLemmatizer {
  val logger = LoggerFactory.getLogger(classOf[LookupLemmatizer])
  val FILENAME_DEFAULT_LOOKUPLEMMATIZER = "annotation/lemmatization-en.txt"

  val lemmatizerHashmap = mutable.Map[String, String]()
  loadLookupLemmatizer(FILENAME_DEFAULT_LOOKUPLEMMATIZER)

  def getLemma(word:String):String = {
    val wordNormalized = word.toLowerCase
    if (!lemmatizerHashmap.contains(wordNormalized)) return wordNormalized
    // Return
    lemmatizerHashmap(wordNormalized)
  }

  def loadLookupLemmatizer(filename:String) = {
    logger.info (" * Loading look-up lemmatizer (" + filename + ")...")

    for(line <- io.Source.fromFile(filename, "UTF-8").getLines()) {
      //println (line)
      val normalized = line.replaceAll("[\\s]+", "\t").trim()
      val split = line.toLowerCase.split("\t")
      val lemma = split(0).trim.toLowerCase
      val word = split(1).trim.toLowerCase

      lemmatizerHashmap += (word -> lemma)
    }
    logger.info (" * Look-up lemmatizer loaded.  (" + lemmatizerHashmap.size + " word -> lemma pairs)" )
  }
}
