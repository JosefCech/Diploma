package Anx

import Xml.XmlReader
import common._
import java.io._
import scala.xml._

object AnxReader extends XmlReader {

  protected def ReadSentence(f : File, use : Boolean) : List[Segment] = {
   val root = ReadFile(f);  
   val segments = root\\"segment" 
   segments.map(t => GetSegment(t,use)).toList
  }
  
  def ReadSentence(f : File ) : List[Segment]= {
    ReadSentence(f,true)
  }
  
  def ReadPureSentence(f : File) : List[Segment]={
    ReadSentence(f,false)
  }
  
  def GetSegment(segment : Node) : Segment = {
    GetSegment(segment,true)
  }
  def GetSegment(segment : Node, use : Boolean) : Segment = {
    val levelString = (segment \\ "@level").toString
    val clauseString = (segment \\ "@clause").toString
    
    val level : Int = {
       if (levelString.toInt >= 0 && use) {
           levelString.toInt
        }
      else{
          -1
       }
    }
  val clause : Int = {
    if (clauseString.toInt >= 0 && use) {
      clauseString.toInt
    }
    else{
      -1
    }
  }
    var segmentGenerate: Segment = null
    val words = (segment\\"word").map(t => CreateWord(t)).toList
    if (words.filter(p => p.isSeparator).size > 0) {
     segmentGenerate = new Boundary(words,level)
    }
    else {
    segmentGenerate = new PureSegment(words,level)
    }
   
    segmentGenerate
  }
  
  def CreateWord(word : Node) : AnxWord = {
    val form : String = (word\\"@form").text.toLowerCase
    val tag : String = (word\\"@tag").text   
    val separator : Boolean = (word\\"@sep").text.trim == "1"
    new AnxWord(form,tag, separator)
  }
    
}