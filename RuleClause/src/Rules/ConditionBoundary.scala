package Rules
import common.AnalyzedSegment
import common.Word
import wordProperties.TagMatcher
/**
 *  class BoundaryCondition contains segment with boundary
 *    
 **/

 class ConditionBoundary extends ConditionBase {
  var boundary : List[(String,String)] = List[(String,String)]()
    
  override def satisfiesSegment(segment : AnalyzedSegment) : Boolean =
  {
     val formMatch : Boolean = this.formMatch(segment)
     val tagMatch : Boolean = this.tagMatch(segment)
    
     (formMatch) && (tagMatch )
  }
  
  protected var forms = this.boundary.map(f => f._1).mkString("", " ", "")
  protected var tags = this.boundary.map(f => f._2).mkString("", " ", "")
  
  protected def formMatch(segment : AnalyzedSegment):Boolean =
  {
     def formMatchSimple(separators : List[Word], boundary : List[String] ) : Boolean = {

       if (separators.isEmpty && boundary.isEmpty) { 
         true // succesfully ends
       }
       else {
         val condition = boundary.head
         val matchForm = { 
	 		if (condition.contains("|")) {
		       condition.split('|').map(t => t.trim).toList.contains(segment.morfWords.head.form.trim)
		     }
	         else if (condition.isEmpty) {
	           true;
	         }
	         else {
	           condition == segment.morfWords.head.form 
	         }
		}
         
        if (matchForm) {
           formMatchSimple(separators.tail,boundary.tail)
        }
        else {
          false
        }
       }
     }
     val formsBoundary = this.boundary.map(t => t._1)
     formMatchSimple(segment.morfWords,formsBoundary)
    
  }
  
  protected def tagMatch(segment : AnalyzedSegment) = {
    	
    def tagMatchSimple(separators : List[Word], boundary : List[String] ) : Boolean = {
        if (separators.isEmpty && boundary.isEmpty) { 
         true // succesfully ends
        }
        else {
          val condition = boundary.head
          val matchTag = {
              if (condition.contains("|")) {
            	condition.split("|").foldLeft(false)((comp,compTag) => ( comp || TagMatcher.Match(segment.morfWords.head.tag, compTag))) 
			  }
			  else if (condition.isEmpty){
			    true
			  }
			  else {
			    TagMatcher.Match(segment.morfWords.head.tag, condition)
			  }
           }
           if (matchTag) {
             tagMatchSimple(separators.tail,boundary.tail)
           }
           else{
             false
           }
        }
      }
    tagMatchSimple(segment.morfWords,this.boundary.map(t => t._2))
  }

  
  override def isEmpty : Boolean = false
  override def toString : String =  "Boundary form : " + this.forms + ", tag : " + this.tags  
 }

