package Vallex

import Xml.XmlReader
import common.MorfWord
import common.Word
import java.io.File
import common.Sentence

object VallexReader extends XmlReader {
  
  def Read(f : File) : List[Sentence] = {
    
   val source  = scala.xml.XML.loadFile(f)
   val sentences = (source\\"s").toList
   
   sentences.map(t => ParsePdtSentence(t))
    
  }
  
 def ParsePdtSentence(sentenceNode : xml.Node) : Sentence = {
    val words = (sentenceNode\\"m").toList
    val ident =  (sentenceNode\"@id").toList.text
    new common.Sentence(words.map(t => CreateWord(t)),ident)
 }
 
 def CreateWord(wordNode : xml.Node) : Word = {
   new  MorfWord((wordNode\\"form").toList.head.text.toLowerCase,(wordNode\\"lemma").toList.head.text,(wordNode\\"tag").toList.head.text) 
 }
}