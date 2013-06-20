package segmenter

import java.io._
import common.sentence.{ Sentence , MorfSentence, AnxSentence , AnalyzedSentence}
import common.{Tree, Directory, AWord, MorfWord, AnalyzedWord}
import common.segment.{Segment, PureSegment}
import Anx.AnxReader
import Pdt.{AReader, MorfReader}

/**
 * segmenter app - create data for testing 
 * for each sentence create own file
 * get only sentence from golden set into own file 
 */
object Segmenter  extends App {
  
    override def main(args: Array[String]) {
      def files = common.Directory.ReadPdtMorfFiles(Configuration.PdtDataFolder).toArray
      def analyticFiles = Directory.ReadAnalyticFiles(Configuration.PdtDataFolder).toArray
      def anxFiles = common.Directory.ReadAnxFiles(Configuration.AnxDataFolder).toList
      def anxSentences = anxFiles.map(t => readAnxFile(t))
      processFiles(files,analyticFiles.toList,anxSentences)
    }
    
    def readAnxFile(file : File) : AnxSentence = {
      AnxReader.ReadSentence(file)
    }
    
     def readMorfFile(file : File): List[MorfSentence] = {
       try {
			  MorfReader.Read(file).toList
			}
			catch  {
			  case e : Exception =>  {
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
     case file : File => {	try {
    	 					  MorfReader.Read(file).toList
    	 					}
     						catch  {
     						  case e : Exception =>  {
     							  		println(file.getAbsolutePath())
     							  		List[MorfSentence]()
			     					  }
     						}
     }
     case file : String =>  { MorfReader.Read(new File(file)).toList
     						}
     }
  
   /**
    * process files from arguments
    */
    def processFiles(morfFiles: Any, aFiles : List[File], anxSentences : List[AnxSentence])  = morfFiles match {
     
      case morfFiles : Array[File] => { 
    	  							def sentences = morfFiles.map(
    	  											t => {
    	  												val morfSentences = this.readMorfFile(t)
    	  											    val trees = this.readAtree(this.getAFile(t,aFiles));
    	  												this.combineInformation(morfSentences,trees,anxSentences)
    	  											    null
    	  											}
    	  											)
    	  							
    	  							morfFiles.head.getCanonicalPath
    	  							// crate trees - tuples index a and tree
    	  							// gp throw all sentences by index
    	  							// 
    	  							//writeAnxFile(sentences)   	  														   	
                                  }
      case _  => /*nothing*/ 
    }
    
    def getAFile(file: File, aFiles : List[File]): File = {
      val nameFile = file.getCanonicalPath
      print(nameFile)
      val nameAFile = nameFile.replaceFirst(".m.",".a.")
      print(nameAFile)
      aFiles.filter(p => p.getCanonicalFile == nameAFile).head
     
      
    }
   
    
    def combineInformation(morfSentences : List[MorfSentence], trees : List[Tree], anxSentences : List[AnxSentence]) : List[AnalyzedSentence] = {
      morfSentences.map(m => {
           val anxSentence = anxSentences.filter(a => a == m)
           val tree = trees.filter(t => m.ident == t.ident)
           createAnalyzedSentence(m.morfWords, anxSentence.head.Segments, tree.head.words) 
      })
      
    }
    
    def createAnalyzedSentence(mWords : List[MorfWord], anxSentence : List[Segment], tWords : List[AWord] ) : AnalyzedSentence = {      
     val words = createAnalyzedWords(mWords,tWords, List[AnalyzedWord]() );
     var countWords = 0;
     val segments =  anxSentence.map(t => t match {
       case   t : PureSegment => MapWordsSegments()
     }
     )
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
	        		 var previousJoined = false
	        		  if (!data.isEmpty){
	        		  val segments = data.zipWithIndex.map(f => 
			        		     { 
			        		       val segment = t.segments.apply(f._2)
			        		       segment.SetLevel(f._1._1)
			        		       segment.SetClause(clauseNum)
			        		       if (f._1._2 == 0 && !previousJoined) {
			        		         clauseNum += 1
			        		       }
			        		       else if (f._1._2 == 0 && previousJoined){
			        		         previousJoined = false
			        		       }
			        		       else if (f._1._2 == 1){
			        		         previousJoined=true
			        		       }
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