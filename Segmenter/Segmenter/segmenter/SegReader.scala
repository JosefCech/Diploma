package segmenter

import scala.io._
import java.io._

/**
 *  SegReader read data from anx files
 *  Argument file is name of file with structure
 *  data like anx-files
 **/
object SegReader {
  
  def ReadData(file : String): Array[(Int,Int)] = {
    val f = new File(file)
    if (f.exists) {  // if exists
    val pattern = "^\\d \\d".r // prepare regexp pattern
	scala.io.Source.fromFile(file).getLines(). // read all lines
	filterNot(p => pattern.findFirstMatchIn(p).isEmpty). // remove empty line 
	map(t => t.split(" ").map(f => f.toInt)). // split into array of ints
	map(t => (t.apply(0),t.apply(1))).toArray  // create pairs instead array of array of ints
    }
    else {
      Array[(Int,Int)]()
    }
    
  }

}