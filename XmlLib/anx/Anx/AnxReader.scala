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
    new PureSegment(words);  
  }
  
  def CreateWord(word : Node) : (String,String) = {
    val form : String = (word\\"@form").text
    val tag : String = (word\\ "@tag").text
    val separator : String = (word\\"@sep").text
    (form,tag)
  }
    
}