package common.sentence

import common.segment.{ Segment, Boundary, PureSegment }
import common.segment.InfoSegment
import common.{ Word }

trait LevelEstimateSentence {
  
  def estimateLevelSegments(segments : List[Segment]) : List[Segment] = {
   
    // if subflag level + 1, previous segment start new clause
    // if boundary == "," level -1, previous segment start newclause , if next has  subflag
       // next try => level max-up
    // if two segments with verb => second start newclause
    estimateLevels(segments,0,List[String](),List[Segment]())
 
  
  }

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
               val info = new InfoSegment(boundary)
               if (!info.HaveCordConjuction) {
                 level += 1
               }
                          
	          val nextSegment = setLevelSegment(segments.tail.head,level, true )
	          estimateLevels(segments.tail.tail,level, newDualBorder , nextSegment :: boundary :: acc)
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
  
  private def containSubflag(s : Segment) : Boolean = s match {
    case s : Boundary => s.haveSubFlag
    case s : PureSegment => s.haveSubFlag
    case _ => false
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
}