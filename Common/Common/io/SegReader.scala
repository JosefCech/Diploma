package io

import scala.io._
import java.io._

object SegReader {
  
  def ReadData(file : String): (String  ,Array[(Int,Int)]) = {
        ReadData(new File(file))  
  }
  
  def ReadData(file : File) : (String  ,Array[(Int,Int)])= {
    if (file.exists) {
    val pattern = "^\\d \\d".r
    val lines =  scala.io.Source.fromFile(file).getLines();
    val data = scala.io.Source.fromFile(file).getLines().filterNot(p => pattern.findFirstMatchIn(p).isEmpty ).map(t => t.split(" ").map(f => f.toInt)).toList
    val ident = scala.io.Source.fromFile(file).getLines().filter(p => (!p.isEmpty && pattern.findFirstMatchIn(p).isEmpty)).toList
    if (!ident.isEmpty && !(ident.head == "null")) {
    (ident.head,data.map(t => (t.apply(0),t.apply(1))).toArray)
    }
    else {
       (file.getName.replace(".seg", ""),data.map(t => (t.apply(0),t.apply(1))).toArray)
    }
    }
    else {
      ("", null)
    }     
  }

}