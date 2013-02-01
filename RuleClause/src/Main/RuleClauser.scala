package Main
import common._

class RuleClauser(val sentence : List[Segment]) {

  def analyzedSegments = sentence.map(t => new AnalyzedSegment(t)).toList 
  def estimationOfClause : Int = {

      val countActiveVerb =  analyzedSegments.filter(t=> t.countWords > 0 && t.haveActiveVerb).size
       if (countActiveVerb == 0) { 
         1 
       }
       else{
          countActiveVerb
       }
      
  }
  
  
  def containsPairBounary(segments : List[AnalyzedSegment]) : Boolean = analyzedSegments.filter(f => f.haveOpeningBracket.size > 0 || f.haveQuotationMark.size > 0).size > 0
  
  def ParsedAccordingPairBoundary(segments : List[AnalyzedSegment]) : Array[(Int,List[AnalyzedSegment])] = {
     
    def ParsingData(segments : List[AnalyzedSegment],
    				level: Int,
    				queue : List[String],
    				accList : List[AnalyzedSegment],
    				accArray : Array[(Int,List[AnalyzedSegment])]
    ) 
    : Array[(Int,List[AnalyzedSegment])]  = {
      if (segments.isEmpty) {
        if (accList.isEmpty) accArray
        else if (!queue.isEmpty) throw new Exception("Parsing Error")
        else  accArray ++ Array((0,accList))
      }
      else {
        val analyzedSegment = segments.head
        if (analyzedSegment.haveCloseBracket.size > 0) {
           val foundBracket = analyzedSegment.haveCloseBracket.head
           val requiredBracked = queue.head
           if (!foundBracket.equals(requiredBracked))
             throw new Exception("Parsing Error - No required bracket")
           
           ParsingData(segments.tail, level-1,queue.tail,List(analyzedSegment),accArray ++ Array((level,accList)))
        }
        else if (analyzedSegment.haveOpeningBracket.size > 0) {
             val foundBracket = analyzedSegment.haveOpeningBracket.head
             val requiredBracket = foundBracket.syntacticOpposite
             
             ParsingData(segments.tail, level+1,queue ::: List(requiredBracket),List[AnalyzedSegment](),accArray ++ Array((level,accList ::: List(analyzedSegment))))
        }
        else {
          
        ParsingData(segments.tail, level,queue,List[AnalyzedSegment](),accArray ++ Array((level,accList ::: List(analyzedSegment))))
        }
        
      }
    }
    ParsingData(segments,0,List[String](),List[AnalyzedSegment](),Array[(Int,List[AnalyzedSegment])]())
  }
    
  def analyzeSentence : List[(Int,AnalyzedSegment)] = {
    
     val firstStep = { if ((this.containsPairBounary(this.analyzedSegments))) {
			           this.ParsedAccordingPairBoundary(this.analyzedSegments)
			         }
     				else {
     				   Array((0,this.analyzedSegments))
     				}
     				 
     }
     
       def useSubflags(pseudoClause : Array[(Int,List[AnalyzedSegment])], segments : List[AnalyzedSegment], level : Int,   acc : List[(Int,AnalyzedSegment)]) : List[(Int,AnalyzedSegment)] = {
        if (segments.isEmpty)
        {
          if (pseudoClause.isEmpty) acc
          else useSubflags(pseudoClause.tail,pseudoClause.head._2,pseudoClause.head._1,acc)
        } 
        else {
          val actualSegment = segments.head
          if (actualSegment.haveSubFlag){
            useSubflags(pseudoClause,segments.tail,level+1,(level+1,actualSegment) :: acc)
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
  }
  
