package common.segment

import common.Interfaces.ITaggedSegment

class TaggedSegment(val segment : Segment) extends ITaggedSegment  {
  private def analyzed = new AnalyzedSegment2(segment)
  
  def GetTag() : String = {
    var tag = ""
    /*Boundary vs Segment   B x C(omplex) x S  */
     tag  += this.GetTypeOfSegment
    /*level */  
     tag += this.GetLevel
    /*contains active verb X - not contains C(ontains)   M*/
    tag += this.GetContainsVerb
    /*contains  reflexive pronouns */
    tag += this.GetReflexivePronouns
    
    /*contains subflag  and  what*/
    tag += this.GetSubflag
    
    /*get Boundary type J x Z x I (JZ) x K (JJ) x L (ZZ) x C (more komplex)*/
    tag += this.GetBoundaryType
    
    return tag;
  }
  /***************************************************************/
  
  private def GetTypeOfSegment : String = {
    if (analyzed.IsBoundarySegment && analyzed.CountWords > 0) {
      "B"
    }
    else if (analyzed.IsBoundarySegment && analyzed.CountWords > 1) {
      "C"
    }
    else {
      "S"   
    }
  }
  
  private def GetLevel : String = {
    analyzed.segment.level.toString
  }
  
  private def GetContainsVerb : String = {
   if (analyzed.HaveActiveVerb)
   {
     "C"
   }
   else {
     "X"
   }
  }

  private def GetReflexivePronouns : String = {
    if (this.analyzed.HaveReflexivePronoun) {
      "C"
    }
    else {
      "X"
    }
   }
 
  private def GetSubflag : String = {
    if (analyzed.HaveSubFlag){
      val tag : String = analyzed.FirstSubflag.tag
      if (tag.startsWith("J")){
        tag.head.toString
      }
      else {
        tag.tail.head.toString
      }
    }
    else {
      "X"
    }
  }
  
  private def GetBoundaryType : String = {
     if (analyzed.IsBoundarySegment) {
       if (analyzed.CountWords == 1){
           analyzed.morfWords.head.tag.head.toString
       }
       else if (analyzed.CountWords == 2) {
         val double =  	analyzed.morfWords.head.tag.head.toString + 
        		 		analyzed.morfWords.tail.head.tag.head.toString()
         double match {
           case "JJ" => "K"
           case "ZJ" => "L"
           case "ZZ" => "M"
           case "JZ" => "N"
           case _ => "U"
         }
       }
       else{
         "C"
       }
     }
     else {
       "X"
     }
  }
}