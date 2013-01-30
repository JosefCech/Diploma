package Anx

import Xml.XmlReader
import common._
import java.io._
import scala.xml._

object AnxReader extends XmlReader {

  def ReadSentence(f : File) : List[Segment] = {
   val root = ReadFile(f);  
   val segments = root\\"segment" 
   segments.map(t => GetSegment(t)).toList
  }
  
  def GetSegment(segment : Node) : Segment = {
    val words = (segment\\"word").map(t => CreateWord(t)).toList
    if (words.filter(p => p.isSeparator).size > 0) {
      new Boundary(words)
    }
    else {
    new PureSegment(words);  
    }
  }
  
  def CreateWord(word : Node) : AnxWord = {
    val form : String = (word\\"@form").text.toLowerCase
    val tag : String = (word\\"@tag").text   
    val separator : Boolean = (word\\"@sep").text.trim == "1"
    new AnxWord(form,tag, separator)
  }
    
}