package segmenter

import java.io._
import common.sentence.{ Sentence , MorfSentence, AnxSentence , AnalyzedSentence}
import common.{Tree, Directory, AWord, MorfWord, AnalyzedWord , ClauseInfo, SegmentInfo}
import common.segment.{Segment, PureSegment, AnalyzedSegment}
import Anx.AnxReader
import Pdt.{AReader, MorfReader}

/**
 * segmenter app - create data for testing 
 * for each sentence create own file
 * get only sentence from golden set into own file 
 */
object Segmenter  extends App {
  
    override def main(args: Array[String]) {
      // read morf files
      def files = common.Directory.ReadPdtMorfFiles(Configuration.PdtDataFolder).toArray
      // read analytic files
      def analyticFiles = Directory.ReadAnalyticFiles(Configuration.PdtDataFolder).toList
      // read anx files
      def anxFiles = common.Directory.ReadAnxFiles(Configuration.AnxDataFolder).toList
      // read anx sentences
      def anxSentences = anxFiles.map(t => readAnxFile(t))
      // add data from morf and analytics to anx
      processFiles(files,analyticFiles)
    }
    
    def readAnxFile(file : File) : AnxSentence = {
      AnxReader.ReadSentence(file)
    }
    
     def readMorfFile(file : File): List[MorfSentence] = {
       try {
			 // Read morf file 
    	   	 MorfReader.Read(file).toList
			}
	   catch {
			  case e : Exception =>  {
			            // print no xml file
				  		println(file.getAbsolutePath())
				  		List[MorfSentence]()
 					  }
			} 
     }
    
     
    def readAtree(file : File) : List[(Tree)] = {
      AReader.CreateTrees(file)    
    } 
    
   /**
    * read whole file with morphologic information (.m)
    */   
   def parsedSegments(file : Any): List[MorfSentence] = file match {
     case file : File => {	this.readMorfFile(file)
     					 }
     case file : String =>  {	this.readMorfFile(new File(file))
     					    }
   }
   
   /**
    * process files from arguments
    */
    def processFiles(morfFiles: Any, aFiles : List[File])  = morfFiles match {
     
      case morfFiles : Array[File] => { 
    	  							def sentences = morfFiles.map(
    	  											t => {
	    	  												// get sentences with morf information
    	  													val morfSentences = this.readMorfFile(t)
    	  													// get analytics data for previous sentences
	    	  											    val trees = this.readAtree(this.getAFile(t,aFiles));
    	  													// combine data from .anx .m .a files
	    	  												this.combineInformation(morfSentences,trees)
    	  											     }
    	  									)
    	  							// write each sentence into own file accord ident		
    	  							writeAnxFile(sentences.flatten.toList)   	  														   	
                                  }
      case _  => /*nothing*/ 
    }
    
    // read analytics data
    def getAFile(file: File, aFiles : List[File]): File = {
      val nameFile = file.getCanonicalPath
      val nameAFile = nameFile.replaceFirst("\\.m","\\.a")
      val result = aFiles.filter(p => {
                                       p.getCanonicalPath == nameAFile
      								  }
                                 )
      
      result.head
     
      
    }
    
    // combine info from files
    def combineInformation(morfSentences : List[MorfSentence], trees : List[Tree]) : List[MorfSentence] = {
      morfSentences.map(m => {
           
           val segData = "" // readDataFromSeg
           val tree = trees.filter(t => m.ident == t.ident)
           if (!tree.isEmpty)
           {
              val clauseWords = createAnalyzedSentence(tree.head.words)
               m.clauseInfo = clauseWords
              
           }
           val data = SegReader.ReadData(Configuration.SegDataFolder + "/" + m.segIdent + ".seg" )
           var clauseNum = 0;
		   var previousJoined : Boolean = false;
		   if (!data.isEmpty){ 
				  val segmentInfo = data.zipWithIndex.map(f => { 
	        		      new SegmentInfo(f._2,f._1._1,f._1._2 == 1);
		        	  }
				  ).toList
		    m.segmentInfo = segmentInfo;
		   }
          m
      })
      
    }
    
 
    def createAnalyzedSentence(tWords : List[AWord] ) : List[ClauseInfo] = {      
     tWords.map( t => {
               new ClauseInfo(t.ident,t.clauseNum)
         }).toList
    }
    
    def createAnalyzedWords(mWords : List[MorfWord],tWords : List[AWord],   acc :List[AnalyzedWord]) : List[AnalyzedWord] = {
      if (mWords.isEmpty) acc.reverse
      else {
         val mWord = mWords.head
         val tWord = tWords.filter(p => p.ident == mWord.ident).head
         val aWord = new AnalyzedWord(mWord, tWord.clauseNum, false)
    	 createAnalyzedWords(mWords.tail,tWords,aWord :: acc);
      }
    }
    
       
    /**
     * write for each sentence into one file with morphologic
     * information without info from golden set
     */
    def writeAnxFile(sentences : List[MorfSentence]) = {
     
      // get into which folder will be written result gold data
      val resultFolder = new File(Configuration.OutputGoldenFolder);
      resultFolder.mkdir()
      
      // folder to write other data
      val resultOthersFolder = new File(Configuration.OutputOthersFolder); 
      resultOthersFolder.mkdir()
      
      // if is possible to all data write
      if (resultFolder.exists && 
          resultFolder.canWrite &&
          resultOthersFolder.exists && 
          resultOthersFolder.canWrite
          ) 
        {
         var clauseInconsistent = 0
         var segmentInconsistent = 0
    	  sentences.foreach(t => {
		       
			  if (t.isClauseAnalyzed)
			  { 
			  
			    if (!t.isClauseConsistent)
			    {
			      clauseInconsistent+= 1;
			    }
			    else if (t.isSegmentLevelAnalyzed && !t.isSegmentLevelConsinstent) 
			    {
			      segmentInconsistent += 1;
			    }
			    else 
			    {
			      if (t.segmentInfo.isEmpty)
			      {
			    	  Anx.AnxWriter.Write( resultOthersFolder.getPath() + '/' + t.segIdent + ".anx", t)
			      }
			      else 
			      {
			          Anx.AnxWriter.Write(resultFolder.getPath + '/' + t.segIdent + ".anx", t)
			      }
			    } 
			  }
			  else {
			    println(t.segIdent)
			    
			  }
		   }
    	  )
    	   println("clause inconsistent " + clauseInconsistent.toString)
    	   println("segment inconsistent " + segmentInconsistent.toString)
    	   
		  
        }
    }
}