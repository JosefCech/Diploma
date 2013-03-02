package segmenter

import Pdt.MorfReader
import java.io._
import common.Sentence
import Anx.AnxWriter


object Segmenter extends App {
  
   def parsedSegments(fileName : String) = {    
       def sentences = MorfReader.Read(new File(fileName)).toList
    sentences
   }
   
    override def main(args: Array[String]) {
     processFiles(args)
    }
    
    def processFiles(files: Array[String]) {
     def sentences = args.map(t => this.parsedSegments(t)).toList.flatten
     writeAnxFile(sentences)
    }
    
    def writeAnxFile(sentences : List[Sentence]) = {
      val resultFolder = new File("Results/");
      resultFolder.mkdir();
      if (resultFolder.exists() && resultFolder.canWrite()) {
      
      sentences.foreach(t => 
        		{ val data = SegReader.ReadData("SegData/"+ t.ident + ".seg" )
        		  //println(data);
        		  Anx.AnxWriter.Write( resultFolder.getPath() + '/' + t.ident + ".anx", t.segments)
        		} )
      }
    }
}