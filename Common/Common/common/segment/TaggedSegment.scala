package common.segment

import common.Interfaces.ITaggedSegment
import common.Tag

class TaggedSegment(val segment : Segment) extends ITaggedSegment  {
  def analyzed = new InfoSegment(segment)
  
  def GetTagString() : String = {
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
    
    /*get Boundary type J x Z x I x P x (,) x (JZ) x K (JJ) x L (ZZ) x C (more komplex)*/
    tag += this.GetBoundaryType
    
    return tag;
  }
  
  def GetTag : Tag = {
    new Tag(this.GetTagString)
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
         analyzed.FirstSubflag.tag.tail.head.toString
      }
    else {
      "X"
    }
  }
  
  private def GetBoundaryType : String = {
     if (analyzed.IsBoundarySegment) {
       if (analyzed.CountWords == 1){
           if (analyzed.morfWords.head.form == ",")
           {
             "P"
           }
           else if (analyzed.morfWords.head.form == "-")
           {
             "D"
           }
           else {
           analyzed.morfWords.head.tag.head.toString
           }
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