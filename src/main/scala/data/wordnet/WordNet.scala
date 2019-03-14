package data.wordnet

import java.io.File
import java.net.URL

//import edu.arizona.sista.embeddings.word2vec.Word2Vec
import edu.mit.jwi._
import edu.mit.jwi.item._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import collection.JavaConversions._

/**
 * Scala wrapper for Java Wordnet Interface (JWI) library
 * Created by peter on 2/18/16.
 *
 * path: path to wordnet_home/dict directory
 */
class WordNet(path:String) {

  // Construct and open dictionary object
  val url = new URL("file:///" + path)
  val dict:IDictionary = new Dictionary(url)
  dict.open()


  def getSense(wordStr:String): Unit = {
    val idxWord:IIndexWord = dict.getIndexWord(wordStr, POS.NOUN)
    val wordID:IWordID = idxWord.getWordIDs.get(0)
    val word:IWord = dict.getWord(wordID)

    println ("id = " + wordID)
    println ("lemma = " + word.getLemma())
    println ("gloss = " + word.getSynset.getGloss())

  }

  def getSynsets(lemma:String, pos:POS):List[ISynset] = {
    val out = new ListBuffer[ISynset]
    val iWord = dict.getIndexWord(lemma, pos)
    if (iWord == null) return List.empty[ISynset]

    val wordIDs = iWord.getWordIDs()
    for (wordID <- wordIDs) {
      val word = dict.getWord(wordID)
      val synset = word.getSynset
      out.append(synset)
    }

    out.toList
  }

  /*
   * Generic function for traversing the relational graph
   */
  def getSynsetsWithRelation(start:ISynset, relation:IPointer):List[ISynset] = {
    val synsets = start.getRelatedSynsets(relation)

    val out = new ListBuffer[ISynset]
    for (synsetID <- synsets) {
      out.append( dict.getSynset(synsetID) )
    }

    out.toList
  }

  /*
   * Functions for retrieving specific relations
   */
  def getHypernyms(start:ISynset):List[ISynset] = {
    getSynsetsWithRelation(start, Pointer.HYPERNYM)
  }

  def getHyponyms(start:ISynset):List[ISynset] = {
    getSynsetsWithRelation(start, Pointer.HYPONYM)
  }

  def getMeronymsMember(start:ISynset):List[ISynset] = {
    getSynsetsWithRelation(start, Pointer.MERONYM_MEMBER)
  }

  def getMeronymsPart(start:ISynset):List[ISynset] = {
    getSynsetsWithRelation(start, Pointer.MERONYM_PART)
  }

  def getMeronymsSubstance(start:ISynset):List[ISynset] = {
    getSynsetsWithRelation(start, Pointer.MERONYM_SUBSTANCE)
  }

  def getHolonymsMember(start:ISynset):List[ISynset] = {
    getSynsetsWithRelation(start, Pointer.HOLONYM_MEMBER)
  }

  def getHolonymsPart(start:ISynset):List[ISynset] = {
    getSynsetsWithRelation(start, Pointer.HOLONYM_PART)
  }

  def getHolonymsSubstance(start:ISynset):List[ISynset] = {
    getSynsetsWithRelation(start, Pointer.HOLONYM_SUBSTANCE)
  }

  def getDefinition(in:ISynset):String = {
    in.getGloss
  }

  def getEntailment(start:ISynset):List[ISynset] = {
    getSynsetsWithRelation(start, Pointer.ENTAILMENT)
  }

  def getRelationTree(start:ISynset, relation:IPointer, maxDepth:Int = 20):List[List[ISynset]] = {
    val out = new ListBuffer[List[ISynset]]

    if (maxDepth <= 0) return List.empty[List[ISynset]]

    // Step 1: Get hypernyms for current level
    val synsets = getSynsetsWithRelation(start, relation)
    //print (" Hypernyms for current relation: ")
    //for (synset <- synsets) println (lemmas(synset).mkString(" "))

    // Step 2: For each relation found, retrieve it's tree
    for (synset <- synsets) {
      val subordinates = getRelationTree(synset, relation, maxDepth-1)
      //println ("  Number of subordinates: " + subordinates.size)
      if (subordinates.size > 0) {
        for (subordinate <- subordinates) {
          out.append(List(synset) ++ subordinate)
        }
      } else {
        out.append(List(synset))
      }
    }

    // Step 3: Return tree from this point
    out.toList
  }

  def getHypernymTree(start:ISynset, maxDepth:Int = 20):List[List[ISynset]] = {
    getRelationTree(start, Pointer.HYPERNYM, maxDepth)
  }

  def getHyponymTree(start:ISynset, maxDepth:Int = 20):List[List[ISynset]] = {
    getRelationTree(start, Pointer.HYPONYM, maxDepth)
  }


  /*
   * Helpers
   */
  def lemmas(in:ISynset):List[String] = {
    val out = new ListBuffer[String]
    val words = in.getWords
    for (word <- words) {
      out.append(word.getLemma)
    }
    out.toList
  }

  def getPOS(in:ISynset):POS = {
    if (in.getType == 1) {
      // Noun
      return POS.NOUN
    } else if (in.getType == 2) {
      // Verb
      return POS.VERB
    } else if (in.getType == 3) {
      // Adjective
      return POS.ADJECTIVE
    } else if (in.getType == 4) {
      // Adverb
      return POS.ADVERB
    } else {
      throw new RuntimeException("ERROR: Unknown POS: " + in.getType)
    }
  }

  def removeSurfaceStopWords(in:Array[String]):Array[String] = {
    val out = new ArrayBuffer[String]
    val stopWords = Array("i", "me", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself",
      "yourselves", "he", "him", "his", "himself", "she", "her", "hers",
      "herself", "it", "its", "itself", "they", "them", "their", "theirs", "themselves",
      "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are",
      "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does",
      "did", "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until",
      "while", "of", "at", "by", "for", "with", "about", "against", "between", "into",
      "through", "during", "before", "after", "above", "below", "to", "from", "up", "down",
      "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here",
      "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more",
      "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so",
      "than", "too", "very", "s", "t", "can", "will", "just", "don", "should", "now")

    for (word <- in) {
      if (!stopWords.contains(word)) out.append(word)
    }

    out.toArray
  }


  /*
   * Word-sense disambiguation
   */

  // Common Lesk method
  def whichSenseLesk(lemma:String, POS:POS, contextSent:Array[String]):Array[(Int, ISynset)] = {
    val senses = getSynsets(lemma, POS)
    val numSenses = senses.size
    val contextSentSet = removeSurfaceStopWords(contextSent).toSet          // Removes duplicate words in context sentence

    val out = new ArrayBuffer[(Int, ISynset)]

    for (i <- 0 until numSenses) {
      var overlap:Int = 0
      val sense = senses(i)
      val definitionWords = sense.getGloss().toLowerCase().replaceAll("""[\p{Punct}]""", " ").split(" ")
      val definitionWordsSet = removeSurfaceStopWords(definitionWords).toSet
      for (contextWord <- contextSent) {
        if (definitionWordsSet.contains(contextWord)) overlap += 1
      }
      out.append( (overlap, sense) )

    }

    val sorted = out.sortBy(- _._1)
    //## Debug
    //for (i <- 0 until sorted.size) {
    //  println (sorted(i)._1 + " " + lemmas(sorted(i)._2) + "  DEFINITION: " + sorted(i)._2.getGloss)
    //}

    // Return
    sorted.toArray
  }


  /*
   * Display / String
   */
  def pathsToString(in:List[List[ISynset]]):String = {
    val os = new StringBuilder

    os.append("Number of paths: " + in.size + "\n")
    for (pathList <- in) {
      os.append("Path length: " + pathList.size + "\n")
      var idx:Int = 1
      for (elem <- pathList) {
        for (i <- 0 until idx) os.append("\t")
        os.append( lemmas(elem).mkString(" ") + "\n")
        idx += 1
      }
      os.append("\n")
    }

    os.toString()
  }

  // Convert a path to a list of lemmas along that path
  def pathsToWordList(in:List[List[ISynset]]):Array[String] = {
    val out = new ArrayBuffer[String]

    for (pathList <- in) {
      for (elem <- pathList) {
        out.insertAll(out.size, lemmas(elem))
      }
    }

    out.toArray
  }

}

object WordNet {



  def main(args: Array[String]) {
    //val wn = new WordNet("/data/nlp/corpora/wordnet/WordNet-3.0/dict-3.1")
    val wn = new WordNet("/home/user/Downloads/dict-3.1")

    wn.getSense("plant")
    println ("")

    val synsets = wn.getSynsets("dish", POS.NOUN)
    for (synset <- synsets) {
      val definition = synset.getGloss
      val lemmas = wn.lemmas(synset)

      println ("Synset")
      println ("Gloss: " + definition)
      println ("Lemmas: " + lemmas.mkString(", "))
      println ("")
    }


    println ("")
    val hypernyms = wn.getHypernyms( wn.getSynsets("dish", POS.NOUN)(0) )
    for (hypernym <- hypernyms) {
      println ("Hypernym: " + wn.lemmas(hypernym))
    }

    println ("")
    println ("Hypernym Tree: ")
    val hypernymTree = wn.getHypernymTree( wn.getSynsets("dog", POS.NOUN)(0) )
    println ( wn.pathsToString(hypernymTree) )


    println ("Hyponym Tree: ")
    val hyponymTree = wn.getHyponymTree( wn.getSynsets("dog", POS.NOUN)(0) )
    println ( wn.pathsToString(hyponymTree) )


    val word = "water"
    println ("Meronyms (" + word + "): ")
    val meronymPart = wn.getMeronymsPart( wn.getSynsets(word, POS.NOUN)(0) )
    val meronymMember = wn.getMeronymsMember( wn.getSynsets(word, POS.NOUN)(0) )
    val meronymSub = wn.getMeronymsSubstance( wn.getSynsets(word, POS.NOUN)(0) )
    println ("Part: ")
    for (meronym <- meronymPart) println("   " + wn.lemmas(meronym).mkString(" "))
    println ("Member: ")
    for (meronym <- meronymMember) println("   " + wn.lemmas(meronym).mkString(" "))
    println ("Substance: ")
    for (meronym <- meronymSub) println("   " + wn.lemmas(meronym).mkString(" "))

    println ("")
    println ("Holonyms (" + word + "): ")
    val holonymPart = wn.getHolonymsPart( wn.getSynsets(word, POS.NOUN)(0) )
    val holonymMember = wn.getHolonymsMember( wn.getSynsets(word, POS.NOUN)(0) )
    val holonymSub = wn.getHolonymsSubstance( wn.getSynsets(word, POS.NOUN)(0) )
    println ("Part: ")
    for (holonym <- holonymPart) println("   " + wn.lemmas(holonym).mkString(" "))
    println ("Member: ")
    for (holonym <- holonymMember) println("   " + wn.lemmas(holonym).mkString(" "))
    println ("Substance: ")
    for (holonym <- holonymSub) println("   " + wn.lemmas(holonym).mkString(" "))


    println ("")
    val dWord = "bank"
    val contextSent = "I went to the bank to desposit the money"
//    val dWord = "dish"
//    val contextSent = "That apple was served on a dish"

    println ("Word/Sense Disambiguation LESK (" + dWord + "): ")
    val contextSent1 = contextSent.toLowerCase.split(" ")
    println ("Context sentence: " + contextSent1.mkString(" "))

    val sortedSenses = wn.whichSenseLesk(dWord, POS.NOUN, contextSent1)
    for (i <- 0 until sortedSenses.size) {
      println (sortedSenses(i)._1 + " " + wn.lemmas(sortedSenses(i)._2) + "  DEFINITION: " + sortedSenses(i)._2.getGloss)
    }


  }

}