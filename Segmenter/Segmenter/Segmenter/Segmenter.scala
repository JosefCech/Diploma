package Segmenter

import Pdt.MorfReader
import java.io._
import common.Sentence
import Anx.AnxWriter


object Segmenter extends App {
  
   def ParsedSegments(fileName : String) = {    
       def sentences = MorfReader.Read(new File(fileName)).toList
    sentences
   }
   
    override def main(args: Array[String]) {
     def sentences = args.map(t => this.ParsedSegments(t)).toList.flatten
     WriteAnxFile(sentences)
	 sentences.foreach(f =>  {println(f.Ident)
	                          f.parsedSegments.foreach(f => println(f.ToString))
	                                      println("")})
    }
    
    def WriteAnxFile(sentences : List[Sentence]) = {
      val resultFolder = new File("Results/");
      resultFolder.mkdir();
      if (resultFolder.exists() && resultFolder.canWrite()) {
      
      sentences.foreach(t => 
        		{ val data = SegReader.ReadData("SegData/"+ t.Ident + ".seg" )
        		  //println(data);
        		  Anx.AnxWriter.Write( resultFolder.getPath() + '/' + t.Ident + ".anx", t.parsedSegments)
        		} )
      }
    }
}