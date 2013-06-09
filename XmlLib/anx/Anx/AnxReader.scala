package Anx

import Xml.XmlReader
import common._
import java.io._
import scala.xml._
import common.sentence.AnxSentence
import common.segment.{ Segment, Boundary, PureSegment }

object AnxReader extends XmlReader {

  protected def ReadSentence(f : File, use : Boolean) : AnxSentence = {
   val root = ReadFile(f);  
   val segments = root\\"segment" 
   new AnxSentence(segments.map(t => GetSegment(t,use)).toList)
  }
  
  def ReadSentence(f : File ) : AnxSentence = {
    ReadSentence(f,true)
  }
  
  def ReadPureSentence(f : File) : AnxSentence ={
    ReadSentence(f,false)
  }
  
  def GetSegment(segment : Node) : Segment = {
    GetSegment(segment,true)
  }
  def GetSegment(segment : Node, use : Boolean) : Segment = {
    
   var level = -1
   var startClause = 0
   if (use){ 
	   val levelString = (segment \\ "@level").toString
	   val startClauseString  = (segment \\ "@clausebeg").toString
	   level  = GetParametr(levelString, -1)
	   startClause = GetParametr(startClauseString,0)
   }
   var segmentGenerate: Segment = null
   val words = (segment\\"word").map(t => CreateWord(t)).toList
   segmentGenerate = new PureSegment(words,level)
   segmentGenerate
  }
  
  def CreateWord(word : Node) : AnxWord = {
    val form : String = (word\\"@form").text.toLowerCase
    val tag : String = (word\\"@tag").text   
    val separator : Boolean = (word\\"@sep").text.trim == "1"
    new AnxWord(form, separator)
  }
 
  private def GetParametr(param : String , default : Int) : Int = {
    try {
      param.toInt
    }
    catch  {
      case _ => default
    }
    
  }
}