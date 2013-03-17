package segmenter

import Pdt.MorfReader
import java.io._
import common.Sentence
import Anx.AnxWriter

/**
 * segmenter app - create data for testing 
 * for each sentence create own file
 * get only sentence from golden set into own file 
 */
object Segmenter  extends App {
  
    override def main(args: Array[String]) {
      def files = common.Directory.ReadPdtMorfFiles(Configuration.PdtDataFolder).toArray;
      processFiles(files)
    }
    
   /**
    * read whole file with morphologic information (.m)
    */
   
   def parsedSegments(file : Any): List[common.Sentence] = file match {
     case file : File => {	try {
    	 						
    	 					MorfReader.Read(file).toList
    	 					}
     						catch  {
     						  case e : Exception =>  {
     							  		
     							  		 println(file.getAbsolutePath())
     							  		 
			     						  List[common.Sentence]()
			     						  }
     						}
     }
     case file : String =>  { MorfReader.Read(new File(file)).toList
     }
     }
   
   
   /**
    * process files from arguments
    */
    def processFiles(files: Any)  = files match {
     
      case files : Array[Any] => { 
    	  							def sentences = files.map(t => this.parsedSegments(t)).toList.flatten
    	  							writeAnxFile(sentences)
    	  														   	
                                  }
      case _  => /*nothing*/ 
    }
    /**
     * write for each sentence into one file with morphologic
     * information without info from golden set
     */
    def writeAnxFile(sentences : List[Sentence]) = {
      val resultFolder = new File(Configuration.OutputGoldenFolder);
      resultFolder.mkdir()
      
      val resultOthersFolder = new File(Configuration.OutputOthersFolder); 
      resultOthersFolder.mkdir()
      
      if (resultFolder.exists && resultFolder.canWrite &&
          resultOthersFolder.exists && resultOthersFolder.canWrite
          ) {
       sentences.foreach(t => 
	        		{ val data = SegReader.ReadData(Configuration.SegDataFolder + "/" + t.segIdent + ".seg" )
	        		 println(Configuration.SegDataFolder + "/" + t.segIdent + ".seg")
	        		 println(new File(Configuration.SegDataFolder + "/" + t.segIdent + ".seg").exists)
	        		 var clauseNum = 0
	        		  if (!data.isEmpty){
	        		  val segments = data.zipWithIndex.map(f => 
			        		     { 
			        		       val segment = t.segments.apply(f._2)
			        		       segment.SetLevel(f._1._1)
			        		       segment
			        		      })
	        		  Anx.AnxWriter.Write( resultFolder.getPath() + '/' + t.segIdent + ".anx", segments.toList)
	        		  }
	        		  else {
	        		     Anx.AnxWriter.Write( resultOthersFolder.getPath() + '/' + t.segIdent + ".anx", t.segments)
	         		  }
	        		} 
	        	)
	      }
	    }
}