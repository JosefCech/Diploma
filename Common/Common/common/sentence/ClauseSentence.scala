package common.sentence

import common.segment.{ InfoSegment , TaggedSegment, Segment }
import common.Tag

trait ClauseSentence {

    /** Estimation count of clause based on active verbs */
 def countEstimate(segments : List[InfoSegment], inBracket : Boolean , countBoundary : Boolean, haveVerb : Boolean , acc : Int) : Int = {
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
  
 def estimateMaxClause(segments : List[Segment]) : Int = segments.map(t => t.clause).toList.max
   
 def estimateClauseNum(segments : List[TaggedSegment]) : List[Segment] = {
    def estimateClauseNum(segments : List[TaggedSegment], previousTag : String, acc:List[Segment]) : List[Segment] = {
      if (segments.isEmpty) acc.reverse
      else {
        val head = segments.head
        val headTag = head.GetTag
        println(headTag.tag)
        if (headTag.isBoundary)
        {
          head.segment.setClause(0)
          estimateClauseNum(segments.tail,headTag.tag, head.segment :: acc)
        }
        else 
       {
          head.segment.setClause(1)
           estimateClauseNum(segments.tail,headTag.tag, head.segment :: acc)
       }
      }
    }
    estimateClauseNum(segments,"", List[Segment]())
 }
 
}