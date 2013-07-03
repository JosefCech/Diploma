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
    def estimateClauseNum(segments : List[TaggedSegment], previousTag : String, actualClause : Int, acc:List[Segment]) : List[Segment] = {
      if (segments.isEmpty) acc.reverse
      else {
        val head = segments.head
        val headTag = head.GetTag
        if (headTag.isBoundary && !acc.isEmpty)
        {
          head.segment.setClause(0)
          estimateClauseNum(segments.tail,headTag.tag, actualClause, head.segment :: acc)
        }
        else if (headTag.isBoundary && 
                 headTag.compare(5, "Z") && 
                 acc.isEmpty
                )
        {
          head.segment.setClause(0)
          estimateClauseNum(segments.tail,headTag.tag, actualClause, head.segment :: acc)
        }        
        else 
       {
          val prevTag : Tag = new Tag(previousTag)
          val prevPureSegment : Tag = getPreviousSegment(acc);
          
          if (( prevTag.Level == prevPureSegment.Level && 
                    prevTag.Level == headTag.Level && 
                    headTag.Level != -1 
                  ) && !(prevPureSegment.haveActiveVerb && headTag.haveActiveVerb))
          {
            acc.head.setClause(actualClause)
          }
          
          val nextClause = {
               actualClause + 1
          }
          
          if (headTag.haveSubflag && !acc.isEmpty)
          {
            head.segment.setClause(nextClause)
            estimateClauseNum(segments.tail,headTag.tag, nextClause, head.segment :: acc)
          }
          else if (prevPureSegment.haveActiveVerb && headTag.haveActiveVerb && !acc.isEmpty)
          {
            head.segment.setClause(nextClause)
            estimateClauseNum(segments.tail,headTag.tag, nextClause, head.segment :: acc)
          }
          else 
          {
	          head.segment.setClause( actualClause)
	          estimateClauseNum(segments.tail,headTag.tag,  actualClause, head.segment :: acc)
          }
       }
      }
    }
    estimateClauseNum(segments,"", 1, List[Segment]())
 }
 
 
private def getPreviousSegment(segments : List[Segment]) : Tag = {
   
    if (segments.isEmpty) new Tag("")
    else {
      val data = new InfoSegment(segments.head)
      if (data.IsBoundarySegment) getPreviousSegment(segments.tail)
      else new TaggedSegment(segments.head).GetTag
    }
   
 }
 
 
}