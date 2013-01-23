package Segmenter

import scala.io._
import java.io._

object SegReader {
  
  def ReadData(file : String): Array[(Int,Int)] = {
    val f = new File(file)
    if (f.exists) {
    val pattern = "^\\d \\d".r
    val data = scala.io.Source.fromFile(file).getLines().filterNot(p => pattern.findFirstMatchIn(p).isEmpty ).map(t => t.split(" ").map(f => f.toInt)).toList
    data.map(t => (t.apply(0),t.apply(1))).toArray
    }
    else {
      Array[(Int,Int)]()
    }
    
  }

}