package segmenter

import Pdt.MorfReader
import java.io._
import common.Sentence
import Anx.AnxWriter

/**
 * Segmenter app - create data for testing 
 * for each sentence create own file
 * get only sentence from golden set into own file 
 */
object Segmenter extends App {
  
    override def main(args: Array[String]) {
     processFiles(args)
    }
    
   /**
    * read whole file with morphologic information (.m)
    */
   def parsedSegments(fileName : String) = {    
       MorfReader.Read(new File(fileName)).toList
   }
   
   /**
    * process files from arguments
    */
    def processFiles(files: Array[String]) {
     def sentences = args.map(t => this.parsedSegments(t)).toList.flatten
     writeAnxFile(sentences)
    }
    /**
     * write for each sentence into one file with morphologic
     * information without info from golden set
     */
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