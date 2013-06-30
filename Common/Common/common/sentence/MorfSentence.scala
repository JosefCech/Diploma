package common.sentence

import common.segment.{ PureSegment, AnalyzedSegment2, AnalyzedSegment, Segment, Boundary }

import common.{Word, MorfWord, ClauseInfo, SegmentInfo}



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

class MorfSentence(val sentence : List[Word], val ident : String  ) {

   var clauseInfo : List[ClauseInfo] = List[ClauseInfo]()
   var segmentInfo : List[SegmentInfo] = List[SegmentInfo]()
  
   
   def this(sentence : List[Word]) = this(sentence,"")
   /** List of MorfWords */
   def morfSentence = sentence.map(
       t => t match {
			case t : MorfWord =>  t 
			case t : Word => new MorfWord(t.form,"")   
			case _ => throw new ClassCastException
            } 
       ).filter(_.tag != "").toList
       
   def morfWords = sentence.map(
       t => t match {
			case t : MorfWord =>  t 
			case t : Word => new MorfWord(t.form,"")   
			case _ => throw new ClassCastException
	        } 
      ).toList
     
   /** List of all boundaries of sentence */
   def getBounderies = {
	   morfSentence.filter(t=> t.isSeparator).toList
   	}
	   
   /** List of all subflags of sentence */                   
   def getSubFlags = {
	   morfSentence.filter( t => t.isSubFlag ).toList	   					  
    }
   
   /** Make simple string from words */
   override def toString =   sentence.map(s => s match {
    case s : Word => s.form
  	}
   ).mkString(" ") 
  
   
   /** List of tuple (Level,Segment) where Level has  value -1 from external source */
   def parsedSegments(levels : List[(Int,Int)]) : List[Segment] = {
     
    def segments(words : List[MorfWord] , acc : List[Segment] , accWord : List[MorfWord], levels : List[(Int,Int)]) : List[Segment] = {
        var restLevels = List[(Int,Int)]();
        var	originLevels =  List[(Int,Int)]();
        var level = -1;
        if (!levels.isEmpty)
        {
          restLevels = levels.tail
          originLevels = levels
          level = levels.head._1
        }
        if (words.isEmpty) { 
          if (!accWord.isEmpty)
          {
           if (accWord.head.isSeparator) 
             (new Boundary(accWord.reverse,level):: acc).reverse
           else {
             (new PureSegment(accWord.reverse,level) :: acc).reverse
           }
          }
          else {
            acc.reverse
          }
        }
        else if (words.head.isSeparator)
         {
          if (accWord.isEmpty) {
           segments(words.tail,(new Boundary(List(words.head),level):: acc),accWord, restLevels)
          }
          else {
            var nextLevel =  -1 
            if (!restLevels.isEmpty){
            nextLevel = restLevels.head._1
            restLevels = restLevels.tail
            }
             segments(words.tail,(new Boundary(List(words.head),nextLevel) :: (new PureSegment(accWord.reverse,level) :: acc)),List[MorfWord](),restLevels)
          }
        }
        else {
           segments(words.tail,acc,words.head :: accWord , originLevels)
        }
      }
     segments(morfSentence,List[Segment](),List[MorfWord](), levels)
   }
  
   /** List of tuple (Level,Segment) where Level has initial value -1 */
   def segments = this.parsedSegments(List[(Int,Int)]())
   
   
   /** List segment with bonus properties on morphological information */
   def morfSegments =  segments.map(t => new AnalyzedSegment2(t)).toList 
   
   /** Estimation count of clause based on active verbs */
   def estimationOfClause : Int = {
     
   def countEstimate(segments : List[AnalyzedSegment2], inBracket : Boolean , countBoundary : Boolean, haveVerb : Boolean , acc : Int) : Int = {
      if (segments.isEmpty){
    	  if (haveVerb) {
    	    acc
    	  }
    	  else{
    	    acc + 1
    	  }
      }
      else {
        if (!inBracket && segments.head.HaveOpeningBracket.size > 0) {
         
           countEstimate(segments.tail,true,countBoundary,haveVerb,acc + 1)
        }
        else if ((!inBracket) && segments.head.HaveCloseBracket.size > 0) {
         
          countEstimate(segments.tail,false,countBoundary,haveVerb,acc + 1)
        }
        else {
        val new_acc : Int = {
         if (countBoundary && segments.head.IsBoundarySegment ){
           if (segments.tail.isEmpty) {
             acc + 1
           }
           else if (segments.tail.head.IsBoundarySegment){
             acc
           }
           else {
             acc +1
           }
         }
         else if (!segments.head.IsBoundarySegment && segments.head.HaveActiveVerb){
           acc + 1
         }
         else if (segments.tail.isEmpty && segments.head.IsBoundarySegment && !haveVerb) {
           acc + 1
         }
         else {
           acc
         }
       }
   
        if (segments.head.IsBoundarySegment) {
          
          countEstimate(segments.tail,inBracket,countBoundary,haveVerb,new_acc)
        }
        else if (segments.head.HaveActiveVerb) {
          countEstimate(segments.tail,false,true,true,new_acc)
        }
        else {
          countEstimate(segments.tail,false,false,haveVerb,new_acc)
        }
        
       }
      }
     }
      countEstimate(this.morfSegments,false, false,false, 0)
    }
   
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
   
   def getClauseNum(data : Segment) : Int =  {
        
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
    
   def getSegmentData(index : Int) : (Int , Boolean) = {
      if (this.segmentInfo.isEmpty)
      {
        (-1, false)
      }
      else {
        (this.segmentInfo.apply(index).level , this.segmentInfo.apply(index).isJoined)
      }
      
    }
   
   def isClauseAnalyzed = !this.clauseInfo.isEmpty
   def isClauseConsistent = this.analyzedSegments.filter(p => p.clause == -1).length > 0
   def isSegmentLevelAnalyzed = !this.segmentInfo.isEmpty
   def isSegmentLevelConsinstent = this.segmentInfo.length == this.segments.length
   
  }
   

