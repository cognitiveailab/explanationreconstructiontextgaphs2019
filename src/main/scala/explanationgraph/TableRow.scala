package explanationgraph

import edu.arizona.sista.struct.Lexicon
import explanationgraph.Table.{ROLE_DATA, ROLE_FILL}
import edu.arizona.sista.processors.{Document, Processor}
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import util.TaggedLemmaHelper
import TableRow.{MODE_WORD, MODE_LEMMA, MODE_TWORD, MODE_TLEMMA, MODE_ANY}

import scala.util.control.Breaks._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Storage class for a single tablerow
  * Note, this storage class includes both String (mostly legacy) and Lexicon indicies (fast) methods.
  * Created by user on 6/30/18.
  */

class TableRow(val table:Table, val cells:Array[String],
               // Cell words, lemmas, tagged-words, and tagged-lemmas, stored as lexicon indices for fast comparison. (cell, ;alternate_idx;, wordIdx)
               val cellWords:Array[Array[Array[Int]]], val cellLemmas:Array[Array[Array[Int]]], val cellTWords:Array[Array[Array[Int]]], val cellTLemmas:Array[Array[Array[Int]]], val cellTags:Array[Array[Array[Int]]]) {

  // Exposed from Table
  val tableName = table.name
  val header = table.header
  val columnRoles = table.columnRoles

  // Internal to TableRow
  val cellWordsStr = new Array[Array[String]](cells.length)
  val rowWordsAllStr = mutable.Set[String]()
  val rowWordsDataOnlyStr = mutable.Set[String]()
  val tableNameHash = tableName.hashCode
  val internalUID = TableRow.getNewUniqueID         // A unique (internal) UID stored as an int, independent from the external UID loaded from the tablestore

  // Reference to TableStore
  val tablestore = table.tablestore
  val lexicon = tablestore.lexicon


  // Step 1: Error checking
  if ((header.size != cells.size) || (header.size != columnRoles.size)) {
    throw new RuntimeException("ERROR: TableRow constructor: Size of header, roles, and/or cells array do not match. ")
  }

  // Step 2: Sanitization
  sanitizeCells()

  // Step 3: Pre-compute cell words
  for (i <- 0 until cells.length) {
    cellWordsStr(i) = cells(i).split(" ")
  }

  // Step 3: Identify UID column
  lazy val uidColumnIdx:Int = table.getUIDColumn()
  lazy val uid:String = getUID()
  lazy val uidHash = uid.hashCode

  // Step 4: Precompute other values that are frequently accessed
  lazy val dataColumns:Array[Int] = table.getDataColumns()
  lazy val dataAndFillColumns:Array[Int] = table.getDataAndFillColumns()

  // Step 5: Precompute row words
  for (colIdx <- dataColumns) {
    for (word <- cellWordsStr(colIdx)) {
      rowWordsDataOnlyStr += word.toLowerCase
    }
  }

  for (colIdx <- dataAndFillColumns) {
    for (word <- cellWordsStr(colIdx)) {
      rowWordsAllStr += word.toLowerCase
    }
  }


  /*
   * Access helpers (getting words from a column)
   */
  // Bag-of-words
  def getCellWordsStr(colIdx:Int):Array[String] = {
    // Split on spaces
    //cells(colIdx).split(" ")    // Compute on-the-fly
    // Precomputed (for speed)
    cellWordsStr(colIdx)
  }

  def getCellWordsStr(colName:String):Array[String] = {
    cellWordsStr( table.getColumnIdx(colName).get )
  }

  def getRowWordsStr():mutable.Set[String] = {
    rowWordsAllStr
  }

  def getRowWordsDataOnlyStr():mutable.Set[String] = {
    rowWordsDataOnlyStr
  }

  // Lexicon text
  def getCellWords(colName:String):Array[Array[Int]] = {
    cellWords( table.getColumnIdx(colName).get )
  }
  def getCellWordsAlt(colName:String, alternative:Int):Array[Int] = {
    cellWords(table.getColumnIdx(colName).get)(alternative)
  }
  def getCellWords(colIdx:Int):Array[Array[Int]] = {
    cellWords( colIdx )
  }
  def getCellWordsAlt(colIdx:Int, alternative:Int):Array[Int] = {
    cellWords(colIdx)(alternative)
  }


  def getCellLemmas(colName:String):Array[Array[Int]] = {
    cellLemmas( table.getColumnIdx(colName).get )
  }
  def getCellLemmasAlt(colName:String, alternative:Int, onlyContentTags:Boolean):Array[Int] = {
    val colIdx = table.getColumnIdx(colName).get
    getCellLemmasAlt(colIdx, alternative, onlyContentTags)
  }
  def getCellLemmas(colIdx:Int):Array[Array[Int]] = {
    cellLemmas( colIdx )
  }
  // TODO: Add this filtering for the other methods (words, tWords, tLemmas)
  def getCellLemmasAlt(colIdx:Int, alternative:Int, onlyContentTags:Boolean):Array[Int] = {
    if (!onlyContentTags) {
      return cellLemmas(colIdx)(alternative)
    } else {
      return filterByContentTag( cellLemmas(colIdx)(alternative), cellTags(colIdx)(alternative) )
    }
  }

  def getCellTWords(colName:String):Array[Array[Int]] = {
    cellTWords( table.getColumnIdx(colName).get )
  }
  def getCellTWordsAlt(colName:String, alternative:Int):Array[Int] = {
    cellTWords(table.getColumnIdx(colName).get)(alternative)
  }
  def getCellTWords(colIdx:Int):Array[Array[Int]] = {
    cellTWords( colIdx )
  }
  def getCellTWordsAlt(colIdx:Int, alternative:Int):Array[Int] = {
    cellTWords(colIdx)(alternative)
  }

  def getCellTLemmas(colName:String):Array[Array[Int]] = {
    cellTLemmas( table.getColumnIdx(colName).get )
  }
  def getCellTLemmasAlt(colName:String, alternative:Int):Array[Int] = {
    cellTLemmas(table.getColumnIdx(colName).get)(alternative)
  }
  def getCellTLemmas(colIdx:Int):Array[Array[Int]] = {
    cellTLemmas( colIdx )
  }
  def getCellTLemmasAlt(colIdx:Int, alternative:Int):Array[Int] = {
    cellTLemmas(colIdx)(alternative)
  }


  def getCellNumAlternatives(name:String):Int = {
    getCellNumAlternatives( table.getColumnIdx(name).get )
  }
  def getCellNumAlternatives(colIdx:Int):Int = {
    cellWords(colIdx).length
  }


  // Plain (ordered) text
  def getCellText(colIdx:Int):String = {
    cells(colIdx)
  }

  def getCellText(colName:String):String = {
    cells( table.getColumnIdx(colName).get )
  }

  // Filter parallel lists of lexicon indices and POS tags to include only those with content tags (e.g. NN, VB, JJ, etc).
  def filterByContentTag(in:Array[Int], tags:Array[Int]) = {
    val keepIdxs = table.contentTagLexiconIdxs
    val out = new ArrayBuffer[Int]
    for (i <- 0 until in.length) {
      if (keepIdxs.contains( tags(i) )) {
        out.append( in(i) )
      }
    }

    // Return
    out.toArray
  }

  private def getUID():String = {
    if (uidColumnIdx >= 0) {
      return cells(uidColumnIdx).toLowerCase
    } else {
      return TableRow.DEFAULT_ROW_UUID
    }
  }

  // A fast way to compare whether the UIDs of two TableRows are equal
  def hasSameUID(in:TableRow):Boolean = {
    // First, try hash (inexpensive test)
    if (this.uidHash != in.uidHash) return false

    // If that matches, verify by comparing the original strings
    if (this.uid != in.uid) return false

    // Return
    true
  }

  // A fast way to compare the internally-generated int UIDs of two TableRows
  def hasSameInternalUID(in:TableRow):Boolean = {
    if (this.internalUID == in.internalUID) return true
    // Return
    false
  }


  /*
   * Access helpers (check if a cell contains a string of words, including alternate forms)
   */


  // Check if the contents of a cell (any alternative) are identical to an input string (provided as a list of lexicon indices).
  // Input: array of lexicon indices
  def doesCellMatchStr(colName:String, in:Array[Int], mode:Int, onlyContentTags:Boolean):Boolean = {
    doesCellMatchStr( table.getColumnIdx(colName).get, in, mode, onlyContentTags)
  }

  def doesCellMatchStr(cellIdx:Int, in:Array[Int], mode:Int, onlyContentTags:Boolean):Boolean = {
    if (mode != MODE_ANY) {
      //## println ("Not any")
      return doesCellMatchStrHelper(cellIdx, in, mode, onlyContentTags)
    } else if (mode == MODE_ANY) {
      //## println ("Mode any")
      if (doesCellMatchStrHelper(cellIdx, in, MODE_WORD, onlyContentTags)) return true
      if (doesCellMatchStrHelper(cellIdx, in, MODE_LEMMA, onlyContentTags)) return true
      if (doesCellMatchStrHelper(cellIdx, in, MODE_TWORD, onlyContentTags)) return true
      if (doesCellMatchStrHelper(cellIdx, in, MODE_TLEMMA, onlyContentTags)) return true
    }

    // Default return
    false
  }

  private def doesCellMatchStrHelper(cellIdx:Int, in:Array[Int], mode:Int, onlyContentTags:Boolean):Boolean = {
    var cellsWithAlts = Array.empty[Array[Int]]
    /*
    println (" * doesCellMatchStrHelper: started... ")
    println ("cellIdx: " + cellIdx)
    println ("in: " + in.mkString(", "))
    println ("Mode: " + mode)
    println ("onlyContentTags: " + onlyContentTags)
    */


    if (mode == MODE_WORD)    cellsWithAlts = cellWords(cellIdx)
    if (mode == MODE_LEMMA)   cellsWithAlts = cellLemmas(cellIdx)
    if (mode == MODE_TWORD)   cellsWithAlts = cellTWords(cellIdx)
    if (mode == MODE_TLEMMA)  cellsWithAlts = cellTLemmas(cellIdx)


    // Comparison
    for (cellWords <- cellsWithAlts) {
      //## println("cellWords: " + cellWords.mkString(", "))

      if (onlyContentTags == false) {
        // Must be an exact match
        if (cellWords.deep == in.deep) return true
      } else {
        // Match is okay as long as content tags are matched.

        var wordIdx:Int = 0
        var inIdx:Int = 0
        breakable {
          while (wordIdx < cellWords.length) {
            // Increment wordIdx until it reaches a content lemma
            while ((wordIdx < cellWords.length) && (!tablestore.isContentLemma(cellWords(wordIdx)))) {
              //## println ("skipping: " + cellWords(wordIdx))
              wordIdx += 1
            }
            if (wordIdx >= cells.length) break()

            // Increment inIdx until it reaches a content lemma
            while ((inIdx < in.length) && (!tablestore.isContentLemma(in(inIdx)))) {
              //## println ("skipping: " + in(inIdx))
              inIdx += 1
            }
            if (inIdx >= in.length) break()

            // Compare words
            if (cellWords(wordIdx) != in(inIdx)) break()

            wordIdx += 1
            inIdx += 1
          }

          // If we reach here, then the two lists of lemmas should have the same content words, in the same order.
          return true
        }

      }

    }

    // Default return
    false
  }



  // Returns a list of all cell alternatives that match the given pattern
  def findCellMatchesStrAlts(colName:String, in:Array[Int], mode:Int, onlyContentTags:Boolean):Array[Int] = {
    findCellMatchesStrAlts( table.getColumnIdx(colName).get, in, mode, onlyContentTags)
  }

  def findCellMatchesStrAlts(cellIdx:Int, in:Array[Int], mode:Int, onlyContentTags:Boolean):Array[Int] = {
    if (mode != MODE_ANY) {
      //## println ("Not any")
      return findCellMatchesStrAltsHelper(cellIdx, in, mode, onlyContentTags)
    } else if (mode == MODE_ANY) {
      //## println ("Mode any")

      val out1 = findCellMatchesStrAltsHelper(cellIdx, in, MODE_WORD, onlyContentTags)
      if (out1.length > 0) return out1

      val out2 = findCellMatchesStrAltsHelper(cellIdx, in, MODE_LEMMA, onlyContentTags)
      if (out2.length > 0) return out2

      val out3 = findCellMatchesStrAltsHelper(cellIdx, in, MODE_TWORD, onlyContentTags)
      if (out3.length > 0) return out3

      val out4 = findCellMatchesStrAltsHelper(cellIdx, in, MODE_TLEMMA, onlyContentTags)
      if (out4.length > 0) return out4
    }

    // Default return
    Array.empty[Int]
  }

  private def checkIfOnlyNonContentTags(in:Array[Int]):Boolean = {
    for (lexiconIdx <- in) {
      if (tablestore.isContentLemma(lexiconIdx)) return false
    }
    // Default return -- if we reach here, there are no content lemmas in this array
    true
  }


  // Returns a list of all cell alternatives that match the given pattern
  private def findCellMatchesStrAltsHelper(cellIdx:Int, in:Array[Int], mode:Int, onlyContentTags:Boolean):Array[Int] = {
    var cellsWithAlts = Array.empty[Array[Int]]
    /*
    println (" * doesCellMatchStrHelper: started... ")
    println ("cellIdx: " + cellIdx)
    println ("in: " + in.mkString(", "))
    println ("Mode: " + mode)
    println ("onlyContentTags: " + onlyContentTags)
    */


    if (mode == MODE_WORD)    cellsWithAlts = cellWords(cellIdx)
    if (mode == MODE_LEMMA)   cellsWithAlts = cellLemmas(cellIdx)
    if (mode == MODE_TWORD)   cellsWithAlts = cellTWords(cellIdx)
    if (mode == MODE_TLEMMA)  cellsWithAlts = cellTLemmas(cellIdx)

    val altMatches = new ArrayBuffer[Int]

    // Check for edge case, where both input and cell content are entirely filtered lemmas
    if (onlyContentTags) {
      if (checkIfOnlyNonContentTags(in)) {
        for (altIdx <- 0 until cellsWithAlts.length) {
          if (checkIfOnlyNonContentTags(cellsWithAlts(altIdx))) {
            altMatches.append(altIdx)
          }
        }
      }
      if (altMatches.length > 0) return altMatches.toArray
    }

    // Normal case
    // Comparison
    //for (cellWords <- cellsWithAlts) {
    for (altIdx <- 0 until cellsWithAlts.size) {
      val cellWords = cellsWithAlts(altIdx)

      //## println("cellWords: " + cellWords.mkString(", "))

      if (cellWords.length > 0) {
        if (onlyContentTags == false) {
          // Must be an exact match
          if (cellWords.deep == in.deep) {
            altMatches.append(altIdx)
          }
        } else {
          // Match is okay as long as content tags are matched.

          var wordIdx: Int = 0
          var inIdx: Int = 0
          breakable {
            while (wordIdx < cellWords.length) {
              // Increment wordIdx until it reaches a content lemma
              while ((wordIdx < cellWords.length) && (!tablestore.isContentLemma(cellWords(wordIdx)))) {
                //## println("skipping: " + cellWords(wordIdx))
                wordIdx += 1
              }
              if (wordIdx >= cellWords.length) break() // new, bugfix?

              // Increment inIdx until it reaches a content lemma
              while ((inIdx < in.length) && (!tablestore.isContentLemma(in(inIdx)))) {
                //## println("skipping: " + in(inIdx))
                inIdx += 1
              }
              if (inIdx >= in.length) break()

              // Compare words
              if (cellWords(wordIdx) != in(inIdx)) break()

              wordIdx += 1
              inIdx += 1
            }

            // If we reach here, then the two lists of lemmas should have the same content words, in the same order.
            //return true
            altMatches.append(altIdx)
          }

        }
      }

    }

    //## println ("altMatches: " + altMatches.toList)
    // Return
    altMatches.toArray
  }


  // Starting from startIdx, see if a given lexical pattern (in) can be matched.
  // If it can, return the index of the last matching element.
  // If it can't be matched, return -1.

  def cellMatchStrSpan(colName:String, altIdx: Int, startFrom: Int, in: Array[Int], mode: Int, onlyContentTags: Boolean): Int = {
    cellMatchStrSpan( table.getColumnIdx(colName).get, altIdx, startFrom, in, mode, onlyContentTags)
  }

  def cellMatchStrSpan(cellIdx: Int, altIdx: Int, startFrom: Int, in: Array[Int], mode: Int, onlyContentTags: Boolean): Int = {
    if (mode != MODE_ANY) {
      //## println ("Not any")
      return cellMatchStrHelperSpan(cellIdx, altIdx, startFrom, in, mode, onlyContentTags)
    } else if (mode == MODE_ANY) {
      //## println ("Mode any")

      val out1 = cellMatchStrHelperSpan(cellIdx, altIdx, startFrom, in, MODE_WORD, onlyContentTags)
      //## println ("out1: " + out1)
      if (out1 > -1) return out1

      val out2 = cellMatchStrHelperSpan(cellIdx, altIdx, startFrom, in, MODE_LEMMA, onlyContentTags)
      //## println ("out2: " + out1)
      if (out2 > -1) return out2

      val out3 = cellMatchStrHelperSpan(cellIdx, altIdx, startFrom, in, MODE_TWORD, onlyContentTags)
      //## println ("out3: " + out1)
      if (out3 > -1) return out3

      val out4 = cellMatchStrHelperSpan(cellIdx, altIdx, startFrom, in, MODE_TLEMMA, onlyContentTags)
      //## println ("out4: " + out1)
      if (out4 > -1) return out4
    }

    // Default return
    -1
  }

  private def cellMatchStrHelperSpan(cellIdx: Int, altIdx: Int, startFrom: Int, in: Array[Int], mode: Int, onlyContentTags: Boolean): Int = {
    var cellWords1 = Array.empty[Int]
    /*
    println("\t * cellMatchStrHelper: started... ")
    println("\tcellIdx: " + cellIdx)
    println("\tin: " + in.mkString(", "))
    println("\tMode: " + mode)
    println("\tonlyContentTags: " + onlyContentTags)
    */


    if (mode == MODE_WORD) cellWords1 = cellWords(cellIdx)(altIdx)
    if (mode == MODE_LEMMA) cellWords1 = cellLemmas(cellIdx)(altIdx)
    if (mode == MODE_TWORD) cellWords1 = cellTWords(cellIdx)(altIdx)
    if (mode == MODE_TLEMMA) cellWords1 = cellTLemmas(cellIdx)(altIdx)


    // Comparison
    //## print("\tcellWords: " + cellWords1.mkString(", ") + "    ")
    for (lexIdx <- cellWords1) print(lexicon.get(lexIdx) + " ")
    //## println ("")

    if (onlyContentTags == false) {
      //println ("condition1")
      if (cellWords1.length <= startFrom + in.length) {
        // Must be an exact match
        if (cellWords1.slice(startFrom, startFrom + in.length).deep == in.deep) {
          //println ("return startFrom+in.length: " + startFrom + in.length)
          return startFrom + in.length
        }
      }
    } else {
      // Match is okay as long as content tags are matched.
      //println ("condition2")

      var wordIdx: Int = startFrom
      var inIdx: Int = 0

      while ((wordIdx < cellWords1.length) && (inIdx < in.length)) {
        //## println ("- iteration")
        //## println ("- wordIdx: " + wordIdx + "  " + lexicon.get(cellWords1(wordIdx)))
        //## println ("- inIdx: " + inIdx + "  " + lexicon.get(in(inIdx)))

        // Increment wordIdx until it reaches a content lemma
        while ((wordIdx < cellWords1.length) && (!tablestore.isContentLemma(cellWords1(wordIdx)))) {
          //## println("skipping: " + cellWords1(wordIdx))
          wordIdx += 1
        }

        // Increment inIdx until it reaches a content lemma
        while ((inIdx < in.length) && (!tablestore.isContentLemma(in(inIdx)))) {
          //## println("skipping: " + in(inIdx))
          inIdx += 1
        }

        //println ("comparison1")
        if (inIdx >= in.length) {
          // If we run out of input string, then we have been successful.
          //## println ("- return wordIdx (1): " + wordIdx)
          return wordIdx + 1
        }

        //println ("comparison2")
        if (wordIdx >= cells.length) {
          // If we have run out of cell text but still have more input string, then we are unsuccessful
          //## println ("- return -1 (1)")
          return -1
        }

        // Compare words: if they're not the same, then there is no match
        //println ("compareWords")
        if (cellWords1(wordIdx) != in(inIdx)) {
          //println ("compareWords false")
          //## println ("- return -1 (3)")
          return -1
        }

        wordIdx += 1
        inIdx += 1

        //## println ("endofloop")
      }

      // If we reach here, then we ran out of cellWords or input words.  Check to see if we've also run out of input words -- if so, then match.
      if ((inIdx >= in.length) && (wordIdx >= cellWords1.length)) {
        // Ran out of both -- pattern should be matched
        return wordIdx
      }
      if ((inIdx >= in.length) && (wordIdx < cellWords1.length)) {
        // We ran out of input words before reaching the end of cellWords -- match
        return wordIdx
      }
      if ((inIdx < in.length) && (wordIdx >= cellWords1.length)) {
        // We ran out of cellWords without matching all of the input words -- no match.
        return -1
      }

    }


    // Default return
    //println ("default return -1")
    return -1
  }



  /*
   * Access helpers (filtering columns)
   */
  /*
  private def getDataColumns():Array[Int] = {
    table.getColumnsByRole( Array(ROLE_DATA) )
  }

  private def getDataAndFillColumns():Array[Int] = {
    table.getColumnsByRole( Array(ROLE_DATA, ROLE_FILL) )
  }

  private def getUIDColumn():Int = {
    for (i <- columnRoles.length-1 to 0 by -1) {
      if (columnRoles(i) == ROLE_UID) {
        return i
      }
    }
    -1
  }


  // Return an array of indicies for all columns that are one of the roles provided in 'roles'.
  // Useful for filtering away API/metadata columns, to get only the data columns, or only data/fill columns, etc.
  def getColumnsByRole(roles:Array[Int]):Array[Int] = {
    val out = new ArrayBuffer[Int]
    for (i <- 0 until columnRoles.size) {
      if (roles.contains(columnRoles(i))) {
        out.append(i)
      }
    }
    out.toArray
  }
  */


  /*
   * Finding columns
   */
  /*
  def getColumnIdx(name:String):Int = {
    table.ColNametoColIdx(name.toUpperCase)
  }
  */


  /*
   * Sanitization
   */
  def sanitizeCells(): Unit = {
    for (i <- 0 until cells.size) {
      cells(i) = cells(i).replaceAll(";", " ; ")
      cells(i) = cells(i).replaceAll("'s", " 's")
      cells(i) = cells(i).replaceAll("\\s+", " ")
    }
  }


  /*
   * toString methods
   */
  override def toString():String = {
    val os = new mutable.StringBuilder()

    for (i <- 0 until cells.size) {
      os.append(cells(i) + "\t")
    }

    os.toString()
  }

  // toString method with custom delimiter, and merges multiple spaces into a single space.  Useful for converting a row into a plain text sentence.
  def toStringDelim(delim:String):String = {
    val os = new mutable.StringBuilder()

    for (i <- 0 until cells.size) {
      os.append(cells(i) + delim)
    }

    os.toString().replaceAll(" +", " ").trim()
  }

  def toStringSentWithUID(delim:String = ""):String = {
    val os = new mutable.StringBuilder()

    val columnIdxs = table.getDataAndFillColumns()
    for (colIdx <- columnIdxs) {
      var text = cells(colIdx)
      if (text.contains(";")) {
        text = "(" + text + ")"
      }
      os.append( text + " " )
    }

    // Add delimiter between row text and tablename/UID marker
    os.append(delim)

    os.append("(" + tableName + ", UID: " + getUID() + ")")

    os.toString().replaceAll(" +", " ").trim()
  }

  // toString method that just displays the sentence text
  def toStringText():String = {
    val os = new mutable.StringBuilder()

    val colIdxs = table.getDataAndFillColumns()
    for (colIdx <- colIdxs) {
      os.append(cells(colIdx) + " ")
    }

    // Return
    os.toString().replaceAll(" +", " ").trim()
  }

  // toString method that just displays the content of the data (i.e. non-fill) columns.  This content will likely not be human readable,
  // and is meant for internal matching (like hashcode generation for automatically generated rows).
  def toStringDataColTextOnly():String = {
    val os = new StringBuilder

    val colIdxs = table.getDataColumns()
    for (colIdx <- colIdxs) {
      os.append(cells(colIdx) + " ")
    }

    // Return
    os.toString().replaceAll(" +", " ").trim()
  }

}


object TableRow {
  lazy val processor:Processor = new CoreNLPProcessor()

  // doesCellMatchStr Modes
  val MODE_WORD       =   1
  val MODE_LEMMA      =   2
  val MODE_TWORD      =   3
  val MODE_TLEMMA     =   4
  val MODE_ANY        =   5

  // default UUID when a UUID hasn't been specified
  val DEFAULT_ROW_UUID  = "No UUID specified!"

  /*
   * Code for generating unique (internal) IDs for each table row
   */
  var uniqueIdCount:Int = 0
  var generateRowCount:Long = 0

  // Generate a unique (and incrementing) internal ID for a given table row
  def getNewUniqueID:Int = {
    uniqueIdCount += 1
    // Return
    uniqueIdCount
  }

  // Get new unique, sequential UIDs for generated rows
  def getNewGenUID(prefix:String = "GEN-"):String = {
    generateRowCount += 1
    prefix + generateRowCount.toString
  }


  // Generator
  def mkTableRow(table:Table, cells:Array[String]):TableRow = {
    val lexicon = table.lexicon

    // Step 1: Create all possible ;alternatives; for this tablerow
    val filterColRoles = Array(ROLE_DATA, ROLE_FILL)
    val cellAlternatives = new Array[Array[String]](cells.length)
    for (i <- 0 until cells.length) {
      if (filterColRoles.contains(table.getColumnRole(i))) {     // Check that this column is a DATA or FILL column
        cellAlternatives(i) = cells(i).split(";")
        // Trim any whitespace
        for (j <- 0 until cellAlternatives(i).length) {
          cellAlternatives(i)(j) = cellAlternatives(i)(j).trim()
        }
        // If cells contain any hyphenated elements (e.g. electric-powered), then make an alternative that doesn't include the hyphenation (e.g. electric powered)
        for (j <- 0 until cellAlternatives(i).length) {
          if (cellAlternatives(i)(j).contains("-")) {
            cellAlternatives(i) ++= Array(cellAlternatives(i)(j).replaceAll("-", " "))
          }
        }
      } else {
        // If this column isn't a DATA or FILL column, then we don't want to annotate the content -- skip over this one.
        cellAlternatives(i) = Array[String]("")
      }
    }

    // Step 1A: Initialize a combination iterator, to iterate through each of these alternatives
    // Determine number of alternatives per cell
    val maxIndices = new Array[Int](cellAlternatives.length)
    for (i <- 0 until cellAlternatives.length) {
      maxIndices(i) = cellAlternatives(i).length
    }

    // Create combination iterator
    val iter = new CombinationIterator(maxIndices)
    val numAlternatives:Int = iter.size.toInt

    // Storage for alternatives
    val wordAlternatives = new Array[Array[Array[Int]]](numAlternatives)
    val lemmaAlternatives = new Array[Array[Array[Int]]](numAlternatives)
    val tWordAlternatives = new Array[Array[Array[Int]]](numAlternatives)
    val tLemmaAlternatives = new Array[Array[Array[Int]]](numAlternatives)
    val tagsAlternatives = new Array[Array[Array[Int]]](numAlternatives)    //##

    var alternativeIdx:Int = 0

    // Step 2: Iterate through all possible combinations
    while (iter.hasNext()) {
      val indices = iter.next()
      val rowStr = new StringBuilder()

      for (cellIdx <- 0 until cells.length) {
        val cellStr = cellAlternatives(cellIdx)(indices(cellIdx))
        if (cellStr.length > 0) {
          rowStr.append( cellStr + " ")
        }
      }

      //##println ("Alternative: " + rowStr.toString())
      val annotation = mkPartialAnnotation( rowStr.toString )
      val sent = annotation.sentences(0)
      /*
      print ("\t")
      for (i <- 0 until sent.words.length) {
        print(sent.lemmas.get(i) + "_" + sent.tags.get(i) + " ")
      }
      println ("")
      */

      // TODO: Match annotation to cell contents (tokenizers are different, so e.g. commas or posessives will need to be tracked). Possibility of multiple spaces.
      //## println ("\n\n ** Span Matching")
      val spans = new Array[(Int, Int)](cellAlternatives.length)    // Locations of each cell in the annotation (word startIdx, word endIdx)

      // Initialize spans to a blank placeholder
      for (i <- 0 until cellAlternatives.length) {
        spans(i) = (-1, -1)
      }

      var atWordIdx:Int = 0
      for (cellIdx <- 0 until cells.length) {
        //## println ("cellIdx: " + cellIdx)
        var cellStr = cellAlternatives(cellIdx)(indices(cellIdx)).trim().toLowerCase()
        var atCellStrIdx:Int = 0


        // Make any transformations to cellStr that the tokenizer has made in the annotation (e.g. parentheses to LRB, RRB)
        cellStr = cellStr.replaceAll("\\(", "-lrb-")
        cellStr = cellStr.replaceAll("\\)", "-rrb-")

        //println ("cellStr: " + cellStr)

        if (cellStr.length > 0) {
          var startIdx = atWordIdx
          var endIdx = atWordIdx

          breakable {
            while (true) {
              // Increment atCellStrIdx to next non-whitespace character
              while ((atCellStrIdx < cellStr.length) && (cellStr.charAt(atCellStrIdx).isWhitespace)) {
                atCellStrIdx += 1
              }

              // Check to see if we're at the end of the cellStr
              if (atCellStrIdx == cellStr.length) {
                break()
              }

              // Check to see if we're at the end of the sentence
              if (atWordIdx >= annotation.sentences(0).words.length) {
                // We're at the end of the words in the annotation
                break()
              }

              // Get next word
              val nextWord = annotation.sentences(0).words(atWordIdx).toLowerCase
              val nextWordLength = nextWord.length

              // Check to make sure enough of the string is left to check the next word
              if (atCellStrIdx + nextWordLength > cellStr.length) break()

              // Try to match next word with next character in this cell
              if (cellStr.substring(atCellStrIdx, atCellStrIdx + nextWordLength) == nextWord) {
                // Match
                endIdx = atWordIdx + 1 // Store new end location
                atWordIdx += 1 // Increment to next word
                atCellStrIdx += nextWordLength

                //##println("Match: " + nextWord)
              } else {

                // No match -- check to make sure we've run out of words in this cell.
                // Case 2: Are we looking at punctuation? (e.g. -LRB- instead of "(" )
                println("* Warning: no match for token: " + nextWord + ". It's likely that the span alignment for this tablerow will be incorrect. Moving onto next cell")

                break()
              }
            }
          }

          // Save cell span indicies
          spans(cellIdx) = (startIdx, endIdx)

        } else {
          // Empty cell
          spans(cellIdx) = (-1, -1)
        }
      }

      /*
      // debug: Display spans
      for (i <- 0 until cells.length) {
        print ( ("(" + spans(i)._1 + ", " + spans(i)._2 + ")".formatted("%12s")) + "\t")
        println ( cellAlternatives(i)(indices(i)).trim().toLowerCase() )
      }
      */

      // TODO: Store as tLemmas/tWords
      val cellWords = new Array[Array[Int]](cells.length)
      val cellLemmas = new Array[Array[Int]](cells.length)
      val cellTWords = new Array[Array[Int]](cells.length)
      val cellTLemmas = new Array[Array[Int]](cells.length)
      val cellTags = new Array[Array[Int]](cells.length)    //##

      for (cellIdx <- 0 until cells.length) {
        if (spans(cellIdx)._1 != -1) {
          val words = new ArrayBuffer[Int]
          val lemmas = new ArrayBuffer[Int]
          val tWords = new ArrayBuffer[Int]
          val tLemmas = new ArrayBuffer[Int]
          val tags = new ArrayBuffer[Int]   //##

          // For each word in the span, generate it's word, lemma, tagged word, and tagged lemma representation, then retrieve/generate the lexicon index for these.
          for (i <- spans(cellIdx)._1 until spans(cellIdx)._2) {
            val word = annotation.sentences(0).words(i)
            val lemma = annotation.sentences(0).lemmas.get(i)
            val tag = annotation.sentences(0).tags.get(i)

            // Add word/lemma/tag to the Lexicon.  This also keeps track of which words are content lemmas.
            table.tablestore.addAllToLexicon(word, lemma, tag)

            // The words should already be added to the lexicon (so .add shouldn't be necessary), but we use it anyway as a lookup to avoid the Option.
            words.append( lexicon.add( word ) )
            lemmas.append( lexicon.add( lemma ) )
            tWords.append( lexicon.add( TaggedLemmaHelper.mkTLemma(word, tag) ) )
            tLemmas.append( lexicon.add( TaggedLemmaHelper.mkTLemma(lemma, tag) ) )
            tags.append( lexicon.add( TaggedLemmaHelper.mkGroupedTag(tag) ) )
          }

          // Store lexicon indices
          cellWords(cellIdx) = words.toArray
          cellLemmas(cellIdx) = lemmas.toArray
          cellTWords(cellIdx) = tWords.toArray
          cellTLemmas(cellIdx) = tLemmas.toArray
          cellTags(cellIdx) = tags.toArray

        } else {
          // Empty cell
          cellWords(cellIdx) = Array.empty[Int]
          cellLemmas(cellIdx) = Array.empty[Int]
          cellTWords(cellIdx) = Array.empty[Int]
          cellTLemmas(cellIdx) = Array.empty[Int]
          cellTags(cellIdx) = Array.empty[Int]
        }
      }

      wordAlternatives(alternativeIdx) = cellWords
      lemmaAlternatives(alternativeIdx) = cellLemmas
      tWordAlternatives(alternativeIdx) = cellTWords
      tLemmaAlternatives(alternativeIdx) = cellTLemmas
      tagsAlternatives(alternativeIdx) = cellTags

      // Increment the alternativeIdx, so we know which row alternative we're working with
      alternativeIdx += 1
    }


    // Step 3: We have to generate alternates by sentence, but we store alternate representations by cell.  Here, find unique cell representations, and store them.
    val uniqueWordAlternatives = new Array[Array[Array[Int]]](cells.length)   // (cell, ;alternate_idx;, wordIdx)
    val uniqueLemmaAlternatives = new Array[Array[Array[Int]]](cells.length)   // (cell, ;alternate_idx;, wordIdx)
    val uniqueTWordAlternatives = new Array[Array[Array[Int]]](cells.length)   // (cell, ;alternate_idx;, wordIdx)
    val uniqueTLemmaAlternatives = new Array[Array[Array[Int]]](cells.length)   // (cell, ;alternate_idx;, wordIdx)
    val uniqueTagAlternatives = new Array[Array[Array[Int]]](cells.length)      // (cell, ;alternate_idx;, wordIdx)

    for (cellIdx <- 0 until cells.length) {
      // TWords will be the most unique, so check this one

      val alternativeIdxs = new ArrayBuffer[Int]
      alternativeIdxs.append(0)


      // Find unique alternatives
      for (altIdx <- 1 until numAlternatives) {
        breakable {
          for (i <- 0 until alternativeIdxs.length) {
            // Compare each known alternative
            if ( tWordAlternatives(altIdx)(cellIdx).deep == tWordAlternatives(alternativeIdxs(i))(cellIdx).deep) {
              // The alternative being examined is the same as a known alternative -- break
              break()
            }
          }
          // If we reach here, the alternative is new
          alternativeIdxs.append(altIdx)
        }
      }

      // Store unique alternatives
      val uWACell = new ArrayBuffer[Array[Int]]
      val uLACell = new ArrayBuffer[Array[Int]]
      val uTWACell = new ArrayBuffer[Array[Int]]
      val uTLACell = new ArrayBuffer[Array[Int]]
      val uTagCell = new ArrayBuffer[Array[Int]]

      //## println (" alternativeIdxs: " + alternativeIdxs.mkString(", "))

      for (altIdx <- alternativeIdxs) {
        uWACell.append( wordAlternatives(altIdx)(cellIdx))
        uLACell.append( lemmaAlternatives(altIdx)(cellIdx))
        uTWACell.append( tWordAlternatives(altIdx)(cellIdx))
        uTLACell.append( tLemmaAlternatives(altIdx)(cellIdx))
        uTagCell.append( tagsAlternatives(altIdx)(cellIdx))
      }

      uniqueWordAlternatives(cellIdx) = uWACell.toArray
      uniqueLemmaAlternatives(cellIdx) = uLACell.toArray
      uniqueTWordAlternatives(cellIdx) = uTWACell.toArray
      uniqueTLemmaAlternatives(cellIdx) = uTLACell.toArray
      uniqueTagAlternatives(cellIdx) = uTagCell.toArray
    }


    /*
    println ("UniqueAlternatives: ")
    for (i <- 0 until cells.length) {
      print (i + ": \t")
      print ("[")
      for (alt <- uniqueTLemmaAlternatives(i)) {
        print("(" + alt.mkString(",") + ") ")
      }
      println ("]")
    }
    println ("\n\n")
    */

    new TableRow(table, cells, uniqueWordAlternatives, uniqueLemmaAlternatives, uniqueTWordAlternatives, uniqueTLemmaAlternatives, uniqueTagAlternatives)
  }


  // Generator for automatically-generated table rows
  def mkTableRowGenerated(table:Table, cellValueLUT:Map[String, String], uuidPrefix:String = "GEN-"):Option[TableRow] = {
    val numColumns = table.header.length
    val columns = Array.fill[String](numColumns)("")

    // Populate columns
    for (colName <- cellValueLUT.keySet) {
      val colIdx = table.getColumnIdx(colName)
      if (colIdx.isEmpty) {
        println ("ERROR: Could not find column '" + colName + "' in table '" + table.name + "'.")
        return None
      }

      // Sanitization (remove multiple spaces, leading/trailing whitespace)
      var field = cellValueLUT(colName).replaceAll(" +", " ").trim()

      // Store
      columns(colIdx.get) = cellValueLUT(colName)
    }

    // Add on a UUID
    columns(table.getUIDColumn()) = getNewGenUID(uuidPrefix)

    // Add row using normal machinery
    val row = mkTableRow(table, columns)

    // Return
    Some(row)
  }

  /*
   * Supporting functions
   */
  def mkPartialAnnotation(text:String):Document = {
    val doc = processor.mkDocument(text)
    processor.tagPartsOfSpeech(doc)
    processor.lemmatize(doc)
    doc.clear()
    doc
  }
}