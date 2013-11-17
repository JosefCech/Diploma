package common.sentence

import common.segment.{ PureSegment,InfoSegment,TaggedSegment, AnalyzedSegment, Segment, Boundary }
import common.{Word, MorfWord, ClauseInfo, SegmentInfo}
import common.segment.TaggedSegment



/** Sentence from list of word. 
  * Base functions : 
  * 		Get words 
  * 		Get all boundaries in sentence
  * 		Get all subflags in sentence
  *         Get segments without levels 
  *         Get segments with my levels
  *                
  * 
  * @constructor create sentence from words and ident (not required)
  * @param sentence all words of sentence (base words with morphologic information)
  * @param ident identificator  
  */

class MorfSentence(val sentence : List[Word], val ident : String  ) extends SegmentedSentence {

   var clauseInfo : List[ClauseInfo] = List[ClauseInfo]()
   var segmentInfo : List[SegmentInfo] = List[SegmentInfo]()
  
   
   def this(sentence : List[Word]) = this(sentence,"")
   /** List of MorfWords */
  
   def morfWords = sentence.map(
       t => t match {
			case t : MorfWord =>  t 
			case t : Word => new MorfWord(t.form,"")   
			case _ => throw new ClassCastException
	        } 
      ).toList
     
   /** List of all boundaries of sentence */
   def getBounderies = {
	   morfWords.filter(t=> t.isSeparator).toList
   	}
	   
   /** List of all subflags of sentence */                   
   def getSubFlags = {
	   morfWords.filter( t => t.isSubFlag ).toList	   					  
    }
   
   /** Make simple string from words */
   override def toString = this.toStringSegments
  
   def toStringSegments = segments.map(s => s.toString).reduce(_ + _ + "\n")
   
   /** List of tuple (Level,Segment) where Level has initial value -1 */
   def segments = this.parsedSegments(this.morfWords, List[(Int,Int)]())
   
   def parsedSegments(levels:List[(Int,Int)]) : List[Segment] = {
     this.parsedSegments(this.morfWords, levels)
   }
   /** List segment with bonus properties on morphological information */
   def taggedSegments =  segments.map(t => new TaggedSegment(t)).toList 
   
  
   /** get ident of sentence from  morf ident */
   def segIdent = this.ident.replace("m-", "")
   
   /** AnalyzedData */
   def analyzedSegments : List[AnalyzedSegment] = {
      this.segments.zipWithIndex.map(f => {
    	 val clauseNum = getClauseNum(f._1)
    	 val segmentData = getSegmentData(f._2)
         val clause = new AnalyzedSegment(f._1,segmentData._1,clauseNum,!segmentData._2)
    	 clause
       }
      )
   }
   
   private def getClauseNum(data : Segment) : Int =  {
        
        val words = data.words.map( w => w match { case w : MorfWord => w }).toList
        def clauseData = this.clauseInfo.filter(
        		c => { 
        			words.filter( 
        			    w => w.ident == c.Ident
        			    ).length == 1
        			 }
        		).filterNot(p => p.ClauseNum == 0).groupBy(f => f.ClauseNum).toList
         if (clauseData.length == 1) {
           clauseData.head._2.head.ClauseNum
         }
         else{
           -1
         }
        }
    
   private def getSegmentData(index : Int) : (Int , Boolean) = {
      if (this.segmentInfo.isEmpty)
      {
        (-1, false)
      }
      else {
        (this.segmentInfo.apply(index).level , this.segmentInfo.apply(index).isJoined)
      }
      
    }
   
   def isClauseAnalyzed = !this.clauseInfo.isEmpty
   def isClauseConsistent = {
     val data = this.analyzedSegments.filterNot(p => p.ClauseNum == -1).
         map(t => (t.LevelDefault, t.ClauseNum)).
         groupBy(_._2).toList.map(t => t._2.groupBy(_._1)).filter(t => t.toList.length > 1)  
   data.length == 0
   }
   def isSegmentLevelAnalyzed = !this.segmentInfo.isEmpty
   def isSegmentLevelConsinstent = this.segmentInfo.length == this.segments.length
   def isClauseSegmentConsistent = false
  }
   

