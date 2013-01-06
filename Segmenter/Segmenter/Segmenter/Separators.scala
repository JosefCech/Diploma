package Segmenter
import Anx._
import Xml._
import scala.xml._
import common._
import java.io._

object Separators extends App {

 def separatorsWords : List[Word]  = {
   
   val rootNode = readData
   val sepXmlWords : List[Node] = (rootNode\\"separator")  toList 
   val sepWords : List[MorfWord] = sepXmlWords.map(t => new MorfWord(t\\"@form" toString , t\\"@tag" toString )).toList
   sepWords
 } 
 
 def subFlags : List[Word] = {
    
   val rootNode = readData
   val subFlagsXml : List[Node] = (rootNode\\"subflag")  toList 
   val subFlagsData : List[MorfWord] = subFlagsXml.map(t => new MorfWord(t\\"@form" toString , t\\"@tag" toString )).toList 
   subFlagsData
 }
   
protected def readData : Node =
		 {
		   val dirData = Configuration.DataFolder
		   val files = Directory.ReadXmlFiles(dirData)
		   val sepFile = dirData +"/"+ files.head.getName()
		   new Xml.Reader(sepFile).data 
		 }
  
}