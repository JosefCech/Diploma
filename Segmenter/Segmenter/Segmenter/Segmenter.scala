package Segmenter

import Pdt.MorfReader
import java.io._
import common.Sentence

object Segmenter extends App {
   
   def parsedSegments = {
       val arg = "Data/cmpr9406_001.m"
       def sentences = MorfReader.Read(new File(arg)).map(t => new Sentence(t)).toList
       def parsedSentences=sentences.take(15)
       parsedSentences
   }
   
    override def main(args: Array[String]) {
      println(args)
	   this.parsedSegments.foreach(f =>  {f.parsedSegments.foreach(f => println(f.ToString))
	                                      println("")})
    }
}