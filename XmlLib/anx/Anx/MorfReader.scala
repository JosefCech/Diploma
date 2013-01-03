package Anx

import common._
import Xml.XmlReader
import java.io._
import scala.xml._

object MorfReader extends XmlReader {
  
  def Read(f : File) : List[List[Word]] = {
    
   val source  = scala.xml.XML.loadFile(f)
   val sentences = (source\\"s").toList
   
   sentences.map(t => ParsePdtSentence(t))
    
  }
  
 def ParsePdtSentence(sentenceNode : xml.Node) : List[Word] = {
    val words = (sentenceNode\\"m").toList
    words.map(t => CreateWord(t))
 }
 
 def CreateWord(wordNode : xml.Node) : Word = {
   new  MorfWord((wordNode\\"form").toList.head.text,(wordNode\\"tag").toList.head.text) 
 }
}