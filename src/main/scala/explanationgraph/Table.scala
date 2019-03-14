package explanationgraph
import Table._
import edu.arizona.sista.struct.Lexicon
import util.TaggedLemmaHelper

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

/**
  * Created by user on 7/9/17.
  */
class Table(filename:String, val tablestore:TableStore, twoLineHeader:Boolean = false) {
  var name:String = ""
  var header = Array.empty[String]
  var columnRoles = Array.empty[Int]
  var rows = new ArrayBuffer[TableRow]
  val UIDtoRowLUT = mutable.Map[String, Int]().withDefaultValue(-1)
  val ColNametoColIdx = mutable.Map[String, Int]().withDefaultValue(-1)

  // Lexicon
  val lexicon = tablestore.lexicon

  // Is this table valid and ready to use?
  var valid = false
  val warnings = new ArrayBuffer[String]

  // Precompute indicies for content tag filtering
  //val contentTags = Array("NN", "VB", "JJ", "IN", "RB", "CD")
  val contentTags = TaggedLemmaHelper.contentTags
  val contentTagLexiconIdxs = new Array[Int](contentTags.length)
  for (i <- 0 until contentTags.length) {
    contentTagLexiconIdxs(i) = lexicon.add( contentTags(i) )
  }




  /*
   Constructor
   */
  loadFromFile(filename, twoLineHeader)


  /*
   Finding rows
   */
  def getRowByUID(uid:String):TableRow = {
    //println ("UID: " + uid)
    val rowIdx = UIDtoRowLUT(uid)
    rows(rowIdx)
  }

  /*
   Finding columns
   */
  def getColumnIdx(name:String):Option[Int] = {
    //println ("name: " + name + " idx: " + ColNametoColIdx(name.toUpperCase))
    val colIdx = ColNametoColIdx(name.toUpperCase)
    if (colIdx == -1) {
      return None   // Failure -- column not found
    }
    // Return
    Some(colIdx)
  }

  // Column role
  def getColumnRole(idx:Int):Int = {
    columnRoles(idx)
  }
   // Column name
  def getColumnName(idx:Int):String = {
    header(idx)
  }


  /*
   * Parse header/rows
   */

  // Read in the table column header (the first line of the tsv file)
  def parseHeader(in:String):Unit = {
    val fields = in.split("\t")
    header = fields
    interpretColumnRoles()

    // Check for valid UID column
    if (findUIDColumnIdx() >= 0) {
      // Found UID column
      valid = true
    } else {
      warnings.append("WARNING: No \"[SKIP] UID\" column found.  This does not appear to be a valid table.")
    }

    // Create the Column Name to Column IDX look-up table
    if (valid) {
      for (i <- 0 until header.size) {
        val colName = header(i).toUpperCase
        ColNametoColIdx(colName) = i

        // Also store a reference to a sanitized version of the name
        ColNametoColIdx( sanitizeColName(colName) ) = i
        //println ("Sanitized: " + sanitizeColName(colName))
      }
    }

  }

  // Sanitize a column name, and convert to uppercase -- for use to make easy IML column references.
  def sanitizeColName(in:String):String = {
    var str = in.toUpperCase
    str = str.replaceAll("[^A-Za-z0-9 ]", " ")   // Remove all non-alphanumeric, non-space characters -- replace them with spaces
    str = str.replaceAll(" +", " ").trim()      // Truncate multiple spaces to a single space
    str = str.replaceAll(" ", "_")              // Replace spaces with underscores
    str = str.toUpperCase

    // Return
    str
  }

  // Read in a table row
  def addRow(in:String):Unit = {
    val fields = in.split("\t")
    val UIDColIdx = findUIDColumnIdx()

    // If row has a UID column, and that UID column is populated, then add the row
    if (UIDColIdx >= 0) {
      // Trim all cells
      for (i <- 0 until fields.size) {
        // Merge multiple spaces
        fields(i) = fields(i).replaceAll(" +", " ")

        // Trim
        fields(i) = fields(i).trim()
      }

      // Check for non-zero content in at least one non-UUID field
      var hasContent:Boolean = true
      breakable {
        for (i <- 0 until fields.size) {
          if ((fields(i).length > 0) && (i != UIDColIdx)) break()
        }
        hasContent = false
      }

      // Add row to table
      val rowUID = fields(UIDColIdx)
      if ((rowUID.length > 0) && (hasContent == true)) {
        //rows.append( new TableRow(this, lexicon, fields) ) // Add row
        rows.append( TableRow.mkTableRow(this, fields) ) // Add row
        UIDtoRowLUT(rowUID) = rows.size - 1 // Add UID for row for fast lookup
      }
    }
  }

  def addRow(in:TableRow):Boolean = {
    // Step 1: Check that the headers are identical -- if not, then not the same table
    if (this.header.deep != in.header.deep) return false

    // Step 2: Check whether this row is a duplicate
    // TODO

    // Step 3: Add row
    rows.append( in )

    // Step 4: Add UID for row for fast lookup
    UIDtoRowLUT(in.uid) = rows.size - 1         // Table LUT
    tablestore.addUIDToTableLUT(name, in.uid)   // TableStore LUT

    // Return
    true
  }

  // Check that a table row is not a duplicate of an existing row
  // Returns (isDuplicate, uuid of duplicate row)
  def isDuplicateRow(in:TableRow):(Boolean, String) = {
    // Step 1: Check that the headers are identical -- if not, then not the same table
    if (this.header.deep != in.header.deep) return (false, "")

    // Step 2: Check the content (non-fill) rows
    val dataCols = getDataColumns()
    for (existingRow <- this.rows) {
      breakable {
        for (colIdx <- dataCols) {
          // TODO: Note, this looks at the raw cell text, and not the alternate forms -- so it's still possible for duplicate
          // alternatives of a given row to exist.  Note also that the comparison is between raw text, not lemmas.
          if (existingRow.getCellText(colIdx).trim().toLowerCase != in.getCellText(colIdx).trim().toLowerCase) {
            // At least one column of text is not identical -- This row is not identical, so break and continue looking at other rows
            break()
          }
        }
        // If we reach here, then all the data columns for one row were identical to the query row ('in').
        return (true, existingRow.uid)
      }
    }

    // If we reach here, the row did not match in the current table, and is unique.
    (false, "")
  }


  // Interpret the roles of each column in the header based off the prefix of the header (fill, skip, uid, etc)
  def interpretColumnRoles():Unit = {
    columnRoles = Array.fill[Int](header.size)(ROLE_UNKNOWN)

    for (i <- 0 until header.size) {
      val colLabel = header(i).trim().toUpperCase
      if (colLabel.startsWith("[FILL]")) {
        columnRoles(i) = ROLE_FILL
      } else if (colLabel.startsWith("[SKIP] UID")) {
        columnRoles(i) = ROLE_UID
      } else if ((colLabel.startsWith("[SKIP]")) || (colLabel.startsWith("#"))) {
        columnRoles(i) = ROLE_SKIP
      } else {
        columnRoles(i) = ROLE_DATA
      }
    }

  }


  /*
   * UID helper functions
   */

  // Return an array of all the UIDs present in this table
  def getAllTableUIDs():Array[String] = {
    UIDtoRowLUT.keySet.toArray
  }

  // Find which column contains the UIDs for each row.  Returns -1 if no UID column was found (signifying an invalid table)
  def findUIDColumnIdx():Int = {
    for (i <- 0 until columnRoles.size) {
      if (columnRoles(i) == ROLE_UID) {
        return i
      }
    }
    // Return
    -1
  }


  /*
   * Accessors
   */
  def numRows():Int = {
    rows.size
  }

  def getRowByIdx(idx:Int):TableRow = {
    rows(idx)
  }


  /*
   * Access helpers (filtering columns)
   */
  def getDataColumns():Array[Int] = {
    getColumnsByRole( Array(ROLE_DATA) )
  }

  def getDataAndFillColumns():Array[Int] = {
    getColumnsByRole( Array(ROLE_DATA, ROLE_FILL) )
  }

  def getUIDColumn():Int = {
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


  /*
   Load from file
   */
  def loadFromFile(filename:String, twoLineHeader:Boolean = false):Unit = {
    name = filenameToName(filename)

    // If the filename is empty, then do not try to load it.
    if (filename.length < 1) return


    //println (" * loadFromFile: Loading table... (filename = " + filename + ") ")

    // Load table header/rows
    var lineCount:Int = 0
    for (line <- io.Source.fromFile(filename, "UTF-8").getLines()) {
      if (twoLineHeader) {
        // Two line header
        if (lineCount == 0) {
          // Do nothing -- this is now a comment line that doesn't need to be parsed
        } else if (lineCount == 1) {
          // Header
          parseHeader(line)
        } else {
          // Rows/data
          if (line.trim.length > 0) {
            addRow(line)
          }
        }
      } else {
        // One line header
        if (lineCount == 0) {
          // Header
          parseHeader(line)
        } else {
          // Rows/data
          if (line.trim.length > 0) {
            addRow(line)
          }
        }
      }

      lineCount += 1
    }
  }


  // Helper: Get name of table by stripping path/extension information from filename
  def filenameToName(in:String):String = {
    // Find beginning trim point
    var lastSlash = in.lastIndexOf("/")
    if (lastSlash < 0) lastSlash = -1
    // Find end trim point
    var period = in.indexOf(".", lastSlash)
    if (period < 0) period = in.length()

    // Return
    in.substring(lastSlash + 1, period)
  }

  /*
   * Removing temporary rows
   */
  def removeTemporaryRows(UUIDPrefix:String = "TEMPGEN-"):Int = {
    var numRemoved:Int = 0

    var idx:Int = 0
    while (idx < rows.length) {
      if (rows(idx).uid.startsWith(UUIDPrefix)) {
        removeRow( rows(idx).uid )
        numRemoved += 1
      } else {
        idx += 1
      }
    }

    // Return
    numRemoved
  }


  def removeRow(uuid:String):Boolean = {
    for (i <- 0 until rows.length) {
      if (rows(i).uid == uuid) {
        tablestore.removeUIDinTableLUT(uuid)      // Remove UUID from UID-Table LUT (used for fast retrieval)
        rows.remove(i)

        // Regenerate UUID-to-Row-index LUT (used for fast retrieval)
        regenerateUUIDRowLUT()

        return true
      }
    }

    // Default return -- if we reach here, a row with the specified UUID could not be found
    false
  }

  private def regenerateUUIDRowLUT(): Unit = {
    UIDtoRowLUT.clear()
    for (i <- 0 until rows.length) {
      UIDtoRowLUT(rows(i).uid) = i
    }
  }

  /*
   * Display
   */
  override def toString:String = {
    val os = new mutable.StringBuilder()

    os.append("Table: " + name.formatted("%35s") + " \tRows: " + rows.size)
    if (valid == false) {
      os.append(" \tValid: " + valid)
    }
    if (warnings.size > 0) {
      os.append(" \tWarnings: " + warnings.mkString(" "))
    }

    os.toString()
  }

  def toStringLong:String = {
    val os = new mutable.StringBuilder()

    os.append("Table: " + name + "  Rows: " + rows.size + "  Valid: " + valid + "  Warnings: " + warnings.mkString(" ") + "\n")
    for (i <- 0 until header.size) {
      os.append( header(i) + " (" + columnRoles(i) + ") \t" )
    }
    os.append("\n")

    for (i <- 0 until rows.size) {
      os.append( "\t" + i + ": " + rows(i).toString + "\n")
    }

    os.toString()
  }

}


object Table {
  /*
   Column Roles
   */
  val ROLE_UNKNOWN    = 0
  val ROLE_DATA       = 1
  val ROLE_FILL       = 2
  val ROLE_SKIP       = 3
  val ROLE_UID        = 4
  val ROLE_API        = 5

}


