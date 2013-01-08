package Segmenter

import common._
import java.io._
import Anx._
import Anx.AnxWriter
import Xml.XmlWritable
import scala.xml._

object SepatatorsGeneate extends App {
  val files = Directory.ReadAllFiles("../../firsttry/SVN/data/pdt/train1",".m")
  
  val data : (List[Word],List[Word]) = GetSeparators(files)
  
  val separators = data._1.groupBy(f => f).keySet.toList
  val subflags = data._2.groupBy(f => f).keySet.toList
  
  def GetSeparators(files : List[File]) : (List[Word],List[Word]) = {
   
      def GetSeparators(files : List[File],acc : List[Word],acc2 : List[Word]) : (List[Word],List[Word]) = {
        if (files.isEmpty) (acc,acc2)
        else {
          try {
          val sentences = Pdt.MorfReader.Read(files.head)
          val seps = sentences.map(f => new Sentence(f)).map(t => t.GetBounderies).toList.flatten.toList
          val sf = sentences.map(f => new Sentence(f)).map(t => t.GetSubFlags).toList.flatten.toList
          GetSeparators(files.tail, acc ::: seps, acc2 ::: sf)
          }
          catch {
              case e: Exception => println("exception caught: " + e);
              GetSeparators(files.tail, acc , acc2 )
            
          }
        }
      }
     GetSeparators(files,List[Word](),List[Word]())
  }
  
  def WriteSeparators(separators : List[Word]) : Boolean = {
	  def CreateSeparatorsNode(separators: List[Word], acc : List[Node]) : Node = {
	    if (separators.isEmpty) <root>{acc}</root>
	    else {
	      val node = <separator>{separators.head.form}</separator>
	      CreateSeparatorsNode(separators.tail,node :: acc)
	    }
	  }
	  
	  val rootNode : Node = CreateSeparatorsNode(separators,List[Node]())
	  print(rootNode)
	  Xml.Writer.Write("separators2.xml",rootNode)
  }
  
  println(subflags.size)
  subflags.foreach(t => println(t.form ))
  
//WriteSeparators(data)
  
}