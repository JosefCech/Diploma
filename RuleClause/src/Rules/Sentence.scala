package Rules
/** Sentence from list of word. This class is inherited from 
 * common.Sentence  
 * Extra functions : 
 * 		
 * 
 * @constructor create sentence from words and ident (not required)
 * @param sentence all words of sentence (base words with morphologic information)
 * @param ident identificator  
 */
import common.{MorfWord,Word,AnalyzedSegment,Segment}

class AnalyzedSentence(sentence : List[Segment])  {

    /** List segment with bonus properties on morphological information */
   def morfSegments : List[AnalyzedSegment] =  sentence.map(t => new AnalyzedSegment(t)).toList 
   
    /** List segment with bonus properties on morphological information - indexed */
   def morfSegmentsIndexed : List[(Int,AnalyzedSegment)] =  sentence.map(t => new AnalyzedSegment(t)).toList.zipWithIndex.map(s => s.swap).toList 
   
   /** Estimation count of clause based on active verbs */
   def estimationOfClause : Int = {
      val countActiveVerb =  morfSegments.filter(t=> t.countWords > 0 && t.haveActiveVerb).size
	  if (countActiveVerb == 0) { 
	     1 
	   }
	   else{
	      countActiveVerb
	   }
    }
 
 
   /** Found pairs boundary */
   private def containsPairBounary(segments : List[AnalyzedSegment]) : Boolean = 
    this.morfSegments.filter(f => f.haveOpeningBracket.size > 0 || f.haveQuotationMark.size > 0).size > 0
  
    /** Get group of segments and its level */
   private def parsedAccordingPairBoundary(segments : List[AnalyzedSegment]) : Array[(Int,List[AnalyzedSegment])] = {
     
   def parsingData(segments : List[AnalyzedSegment],
    				level: Int,
    				queue : List[String],
    				accList : List[AnalyzedSegment],
    				accArray : Array[(Int,List[AnalyzedSegment])]
    ) 
    : Array[(Int,List[AnalyzedSegment])]  = {
      if (segments.isEmpty) {
        if (accList.isEmpty){
          accArray
        }
        else if (!queue.isEmpty) { 
          throw new Exception("Parsing Error")
        }
        else {
          accArray ++ Array((0,accList))
        }
      }
      else {
        val analyzedSegment = segments.head
        if (analyzedSegment.haveCloseBracket.size > 0) {
           val foundBracket = analyzedSegment.haveCloseBracket.head
           val requiredBracked = queue.head
           if (!foundBracket.equals(requiredBracked)){
             throw new Exception("Parsing Error - No required bracket")
           }
           
           parsingData(segments.tail, level-1,queue.tail,List(analyzedSegment),accArray ++ Array((level,accList)))
        }
        else if (analyzedSegment.haveOpeningBracket.size > 0) {
             val foundBracket = analyzedSegment.haveOpeningBracket.head
             val requiredBracket = foundBracket.syntacticOpposite
             
             parsingData(segments.tail, level + 1,queue ::: List(requiredBracket),List[AnalyzedSegment](),accArray ++ Array((level,accList ::: List(analyzedSegment))))
        }
        else {
          
        parsingData(segments.tail, level,queue,List[AnalyzedSegment](),accArray ++ Array((level,accList ::: List(analyzedSegment))))
        }
        
      }
    }
    parsingData(segments,0,List[String](),List[AnalyzedSegment](),Array[(Int,List[AnalyzedSegment])]())
  }
  
    /** Using pair of boundary and subflags to setting each segment its level */
  def analyzedSentence : List[(Int,AnalyzedSegment)] = {
    
     val firstStep = { if ((this.containsPairBounary(this.morfSegments))) {
			           this.parsedAccordingPairBoundary(this.morfSegments)
			         }
     				else {
     				   Array((0,this.morfSegments))
     				}

     }
     
       def useSubflags(pseudoClause : Array[(Int,List[AnalyzedSegment])], segments : List[AnalyzedSegment], level : Int,   acc : List[(Int,AnalyzedSegment)]) : List[(Int,AnalyzedSegment)] = {
        if (segments.isEmpty)
        {
          if (pseudoClause.isEmpty) { 
            acc
          }
          else { 
            useSubflags(pseudoClause.tail,pseudoClause.head._2,pseudoClause.head._1,acc)
          }
        } 
        else {
          val actualSegment = segments.head
          if (actualSegment.haveSubFlag){
            useSubflags(pseudoClause,segments.tail,level + 1,(level + 1,actualSegment) :: acc)
          }
          else {
            useSubflags(pseudoClause,segments.tail,level,(level,actualSegment) :: acc)
          }
          
        }
       }
        
       val secondStep = useSubflags(firstStep,List[AnalyzedSegment](),0,List[(Int,AnalyzedSegment)]()) 
       val data = secondStep
   
       data
    }
  
   def getLevels : List[Int] = this.analyzedSentence.map(t => t._1).toList
}
