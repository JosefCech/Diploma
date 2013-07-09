package common.sentence

import common.segment.{ Segment, Boundary, PureSegment }
import common.segment.InfoSegment
import common.{ Word }
import common.segment.TaggedSegment
import wordProperties.TagMatcher
import common.segment.BaseSegment

trait LevelEstimateSentence {
  
  def estimateLevelSegments(segments : List[Segment]) : List[Segment] = {
   
    // if subflag level + 1, previous segment start new clause
    // if boundary == "," level -1, previous segment start newclause , if next has  subflag
       // next try => level max-up
    // if two segments with verb => second start newclause
    estimateLevels(segments,0,List[String](),List[Segment]())
 
  
  }
  
  var coordConj : Boolean = false
  
  private def estimateLevels(segments : List[Segment], actualLevel : Int, dualBorder : List[String], acc : List[Segment]) : List[Segment] = {
   
    if (segments.isEmpty) acc.reverse
    else {
    if (segments.tail.isEmpty && segmentBoundary(segments.head)) 
    {
      var boundary =  setLevelSegment(segments.head,0, false )
       
      estimateLevels(segments.tail, actualLevel, dualBorder, boundary :: acc)
    }
    else if (segmentBoundary(segments.head)) {
    	 var boundary =  setLevelSegment(segments.head,actualLevel, false )
         val opposite =  boundary.words.head.syntacticOpposite
         var level = actualLevel
         val info = new InfoSegment(boundary)
    	 val prevHaveVerb = {
    	   if (acc.isEmpty) false
    	   else new InfoSegment(acc.head).HaveActiveVerb
    	 }
    	  val prevHaveSubflag = {
    	   if (acc.isEmpty) false
    	   else new InfoSegment(acc.head).HaveSubFlag
    	 }
         val newDualBorder : List[String] = {
                 if (isRequiredDualBorder(boundary,dualBorder))
                 {
                   level -= 1
                   boundary.setLevel(level)
                   dualBorder.tail
                 }
                 else if (opposite.isEmpty) 
                 {
                    dualBorder
                 }
                 else 
                 {
                   level += 1
                   opposite :: dualBorder
                 }
              }
         if (nextSegmentSubflag(segments.tail))
	    	{
                val prev = if (acc.isEmpty) new InfoSegment(BaseSegment.createEmptySegment)
                           else new InfoSegment(acc.head)
                 val next = new InfoSegment(segments.tail.head)
                
               if (!info.HaveCordConjuction && (this.isCommaSegment(boundary) && !acc.isEmpty && this.checkSubFlagsSegment(prev,next))) {
                 level += 1
                 this.coordConj = false;
                 boundary.setLevel(0)                    
               }
	    	                   
	          val nextSegment = setLevelSegment(segments.tail.head,level, true )
	          estimateLevels(segments.tail.tail,level, newDualBorder , nextSegment :: boundary :: acc)
	    	}
         else if ((info.HaveComma) && nextSegmentCordConjuction(segments.tail))
         {
           val nextLevel = {
             if (level == 0) {
               0
             }
             else {
              level-1
             }
           }
           this.coordConj = true;
           boundary.setLevel(0)           
           estimateLevels(segments.tail, nextLevel , newDualBorder, boundary :: acc)
         }
         else if (prevHaveSubflag && prevHaveVerb && nextSegmentActiveVerb(segments.tail) && info.HaveComma)
         {
         val nextLevel = {
             if (level == 0) {
               0
             }
             else {
              level-1
             }
           } 
          this.coordConj = false;
         boundary.setLevel(0)           
         estimateLevels(segments.tail, nextLevel , newDualBorder, boundary :: acc)
           
         }
         
         else if (info.HaveComma && this.coordConj)
         {
           val nextLevel = {
             if (level == 0) {
               0
             }
             else {
              level-1
             }
           } 
          this.coordConj = false;
         boundary.setLevel(0)           
         estimateLevels(segments.tail, nextLevel , newDualBorder, boundary :: acc)
         }
	     else
	        {
	           
	            estimateLevels(segments.tail, level, newDualBorder, boundary :: acc)
	        }
        }
    else {
       var level = actualLevel
       if (nextSegmentSubflag(segments))
       {
         level+=1;
       }

       if (!acc.isEmpty)
       {
         val head = new InfoSegment(acc.head)
         if (head.IsBoundarySegment && head.segment.level != level &&  nextSegmentActiveVerb(segments)){
           if (head.HaveCordConjuction)
           {
            this.coordConj = true;
           }
          acc.head.setLevel(0)
         }
       }
        
        val nextSegment = setLevelSegment(segments.head,level,false)
        estimateLevels(segments.tail, actualLevel, dualBorder, nextSegment :: acc)
    }
   }
  }
  
  private def segmentBoundary(s : Segment) : Boolean = s match
  {
    case s : Boundary => true
    case _ => false
  }
  
  private def nextSegmentSubflag(l : List[Segment]) : Boolean = 
  {
   if (l.isEmpty ) false
   else {
	   containSubflag(l.head)
   }
  }
  
  private def containSubflag(s : Segment) : Boolean = {
   val data = new InfoSegment(s)
   data.HaveSubFlag
  }

  private def setLevelSegment(s : Segment, level : Int , startNewClause : Boolean) : Segment = {
    s.setLevel(level)
    if (startNewClause) s.setStartNewClause()
    s
  }
  
  private def nextSegmentBoundary(l : List[Segment]) : Boolean = 
  { 
     if (l.isEmpty ) false
     else {
	 l.head.isInstanceOf[Boundary];
     }
  }
  
  private def nextSegmentCordConjuction(l : List[Segment]) : Boolean ={
    if (nextSegmentBoundary(l) &&  (new InfoSegment(l.head).HaveCordConjuction))
    {
       val coords = List("a","i")
       l.head.words.filter(p => coords.contains(p.form)).length > 0
    }
    else {
      false
    }
  }
  
  private def nextSegmentActiveVerb(l : List[Segment]) : Boolean = {
    if (l.isEmpty) {
      false
    }
    else {
      new InfoSegment(l.head).HaveActiveVerb
    }
  }
  
  private def isCommaSegment(s : Segment) : Boolean = 
    {
    s.words.filter(p => p.form == ",").length > 0
    }
   
  private def isRequiredDualBorder(s : Segment, b : List[String]) : Boolean =
  {
    if (b.isEmpty) false
    else 
    {
	    val actualBorder = s.words.head.form
	    val requiredBorder = b.head
	    actualBorder == requiredBorder
    }
  }
  
  private def checkSubFlagsSegment(prev : InfoSegment , next : InfoSegment) : Boolean = {
    
    if (prev.HaveSubFlag && next.HaveSubFlag)
    {
      if (!next.FirstSubflag.tag.startsWith("P") && !TagMatcher.Match(prev.FirstSubflag, next.FirstSubflag.tag.head.toString)) {
	       false                 
         } else {
        true
      }
    } else { 
      true
    }
  }
}