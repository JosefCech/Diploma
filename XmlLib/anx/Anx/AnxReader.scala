package Anx

import Xml.XmlReader
import common._
import java.io._
import scala.xml._
import common.sentence.AnxSentence
import common.segment.{ Segment, AnalyzedSegment, PureSegment, Boundary }
import common.MorfWord

object AnxReader extends XmlReader {

  protected def ReadSentence(f : File, use : Boolean) : AnxSentence = {
   val root = ReadFile(f);  
   val segments = root\\"segment" 
   new AnxSentence(segments.map(t => GetSegment(t,use)).toList,f.getName.replaceFirst("\\.anx", ""))
  }
  
  def ReadSentence(f : File ) : AnxSentence = {
    ReadSentence(f,true)
  }
  
  def ReadAnalyzedSentence(f : File) : AnxSentence = {
     val root = ReadFile(f);  
   val segments = root\\"segment" 
     new AnxSentence(segments.map(t => GetAnalyzedSegment(t)).toList,f.getName.replaceFirst("\\.anx", ""))
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
   var clauseNum = -1
   if (use)
   {
	   val levelString = (segment \\ "@level").toString
	   val startClauseString  = (segment \\ "@clausebeg").toString
	   val clauseNumString = (segment \\ "@clauseNum").toString
	   level  = GetParametr(levelString, -1)
	   startClause = GetParametr(startClauseString,0)
	   clauseNum = GetParametr(clauseNumString,-1);
   }
   var segmentGenerate: Segment = null
   val words = (segment\\"word").map(t => CreateMorfWord(t)).toList
   segmentGenerate = new PureSegment(words,level , startClause != 0)
   segmentGenerate.setClause(clauseNum)
   segmentGenerate
  }
  
  def GetAnalyzedSegment(segment : Node) : Segment = 
  {
   val dataSegment = this.GetSegmentData(segment);
   val words = (segment\\"word").map(t => CreateMorfWord(t)).toList
   var newSegment: Segment = null
     val clauseNum : Int = {
     if (dataSegment._2 == -1) 0
     else dataSegment._2
   }
   if (words.filter(p => p.isSeparator).length > 0)
   {
      newSegment = new Boundary(words,dataSegment._1)
      
   }
   else 
   {
    newSegment = new PureSegment(words,dataSegment._1)
   } 
   newSegment.setClause(clauseNum)
  
   val analyzedSegment = new AnalyzedSegment(newSegment, dataSegment._1, clauseNum, dataSegment._3 )
   analyzedSegment
  }
  
  def CreateWord(word : Node) : AnxWord = {
    val form : String = (word\\"@form").text.toLowerCase
    val tag : String = (word\\"@tag").text   
    val separator : Boolean = (word\\"@sep").text.trim == "1"
    new AnxWord(form, separator)
  }
  
    def CreateMorfWord(word : Node) : MorfWord = {
    val form : String = (word\\"@form").text.toLowerCase
    val tag : String = (word\\"@tag").text   
    val separator : Boolean = (word\\"@sep").text.trim == "1"
    new MorfWord(form, tag)
  }
 
  private def GetSegmentData(segment : Node) : (Int,Int,Boolean) = {
   
   val levelString = (segment \\ "@level").toString
   val startClauseString  = (segment \\ "@clausebeg").toString
   val clauseNumString = (segment \\ "@clause").toString
   
    ( GetParametr(levelString, 0),
      GetParametr(clauseNumString,0),
      GetBoolParametr(startClauseString,false)
    )
  } 
  private def GetParametr(param : String , default : Int) : Int = {
    try {
      param.toInt
    }
    catch  {
      case _ => default
    }
    
  }
  private def GetBoolParametr(param: String, default : Boolean) : Boolean = {
     try {
      param == "true"
    }
    catch  {
      case _ => default
    }
    
  }
}