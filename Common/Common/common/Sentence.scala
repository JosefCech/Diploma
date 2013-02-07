package common

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

class Sentence(val sentence : List[Word], val ident : String  ) {

   def this(sentence : List[Word]) = this(sentence,"")
   /** List of MorfWords */
   def morfSentence = sentence.map(
       t => t match {
			case t : MorfWord =>  t 
			case t : Word => new MorfWord(t.form,"")   
			case _ => throw new ClassCastException
            } 
       ).filter(_.tag != "").toList
       
   private def morfWords = sentence.map(
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
   def morfSegments =  segments.map(t => new AnalyzedSegment(t)).toList 
   
   /** Estimation count of clause based on active verbs */
   def estimationOfClause : Int = {
     
     def countEstimate(segments : List[AnalyzedSegment], inBracket : Boolean , countBoundary : Boolean, haveVerb : Boolean , acc : Int) : Int = {
      if (segments.isEmpty){
    	  if (haveVerb) {
    	    acc
    	  }
    	  else{
    	    acc + 1
    	  }
      }
      else {
        if (!inBracket && segments.head.haveOpeningBracket.size > 0) {
         
           countEstimate(segments.tail,true,countBoundary,haveVerb,acc + 1)
        }
        else if ((!inBracket) && segments.head.haveCloseBracket.size > 0) {
         
          countEstimate(segments.tail,false,countBoundary,haveVerb,acc + 1)
        }
        else {
        val new_acc : Int = {
         if (countBoundary && segments.head.isBoundarySegment ){
           if (segments.tail.isEmpty) {
             acc + 1
           }
           else if (segments.tail.head.isBoundarySegment){
             acc
           }
           else {
             acc +1
           }
         }
         else if (!segments.head.isBoundarySegment && segments.head.haveActiveVerb){
           acc + 1
         }
         else if (segments.tail.isEmpty && segments.head.isBoundarySegment && !haveVerb) {
           acc + 1
         }
         else {
           acc
         }
       }
   
        if (segments.head.isBoundarySegment) {
          
          countEstimate(segments.tail,inBracket,countBoundary,haveVerb,new_acc)
        }
        else if (segments.head.haveActiveVerb) {
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
 
}
