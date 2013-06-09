package Pdt

import Xml.XmlReader
import common.MorfWord
import common.Word
import java.io.File
import common.sentence.MorfSentence

object MorfReader extends XmlReader {
  
  def Read(f : File) : List[MorfSentence] = {
    
   val source  = scala.xml.XML.loadFile(f)
   val sentences = (source\\"s").toList
   
   sentences.map(t => ParsePdtSentence(t))
    
  }
  
 def ParsePdtSentence(sentenceNode : xml.Node) : MorfSentence = {
    val words = (sentenceNode\\"m").toList
    val ident =  (sentenceNode\"@id").toList.text
    new MorfSentence(words.map(t => CreateWord(t)),ident)
 }
 
 def CreateWord(wordNode : xml.Node) : Word = {
   new  MorfWord((wordNode\\"form").toList.head.text.toLowerCase,(wordNode\\"lemma").toList.head.text,(wordNode\\"tag").toList.head.text,(wordNode\"@id").toList.head.text) 
 }
}