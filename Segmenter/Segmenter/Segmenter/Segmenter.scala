package Segmenter

import Pdt.MorfReader
import java.io._

object Segmenter extends App {
   
   def parsedSegments = {
       val arg = "Data/cmpr9406_001.m"
       def sentences = MorfReader.Read(new File(arg))
       def sentence = sentences.toList.head
       print(sentence)
       sentences.take(5)
       
   }
   
   this.parsedSegments.foreach(f => println(f))

}