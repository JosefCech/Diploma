package Segmenter

import Pdt.MorfReader
import java.io._

object Segmenter extends App {
   
  
   // base delimiter
   def delimiterWords =List[String](",",":","(",")","\"", "!" , "?");
   // delimiter sub
   def sfWords = ("jak|kam|kde|kdy|kudy|i|a|ale|nebo|protože").split("|");
 
 
   def IsDelimiterWord(word : String ) : Boolean = {
     return delimiterWords.contains(word) 
   }
   
   def IsSubFlag(word : String) : Boolean = {
     return sfWords.contains(word);
   }
   

  
  
   def parsedSegments = {
       val arg = "Data/cmpr9406_001.m"
       def sentences = MorfReader.Read(new File(arg))
       def sentence = sentences.toList.head
       print(sentence)
       sentences
       
   }
   
   

}