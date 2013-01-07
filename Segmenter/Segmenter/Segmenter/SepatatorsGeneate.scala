package Segmenter

import common._
import java.io._
import Anx._
import Anx.AnxWriter
import Xml.XmlWritable
import scala.xml._

object SepatatorsGeneate extends App {
  val files = Directory.ReadAnxFiles("DataAnx")
  
  val data = GetSeparators(files).groupBy(f => f).keySet.toList
  
  def GetSeparators(files : List[File]) : List[Word] = {
   
      def GetSeparators(files : List[File],acc : List[Word]) : List[Word] = {
        if (files.isEmpty) acc
        else {
          val sentence = AnxReader.ReadSentence(files.head)
         
          val seps = sentence.map(f => f match { case f : PureSegment => f.separators}).toList.flatten.groupBy(f => f ).keySet.toList
          GetSeparators(files.tail, acc ::: seps)
        }
      }
     GetSeparators(files,List[Word]())
  }
  
  def WriteSepparators(separators : List[Word]) : Boolean = {
	  def CreateSeparatorsNode(separators: List[Word], acc : List[Node]) : Node = {
	    if (separators.isEmpty) <root>{acc}</root>
	    else {
	      val node = <separator>{separators.head.form}</separator>
	      CreateSeparatorsNode(separators.tail,node :: acc)
	    }
	  }
	  
	  val rootNode : Node = CreateSeparatorsNode(separators,List[Node]())
	  Xml.Writer.Write("separators.xml",rootNode)
  }

  
}