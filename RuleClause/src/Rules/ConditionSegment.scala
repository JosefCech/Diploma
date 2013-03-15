package Rules
import common.AnalyzedSegment

/**
 *  trait  Rule should represented single different rule in each instance
 *    
 **/

 class ConditionSegment extends ConditionBase {
   var attributes : List[(String,String)] = List[(String,String)]()

   override def satisfiesSegment(segment : AnalyzedSegment) : Boolean = {
     
      def compareProperties(segment : AnalyzedSegment, attribs : List[(String,String)]) : Boolean = {
       if (attribs.isEmpty) {
         true
       }
       else {
	       val head = attribs.head
	       val matchVal = head._1 match {
					         case "ActiveVerb"  => ((head._2.toInt != 0) == (segment.haveActiveVerb == true))
					         case "ReflexivePron" => (head._2.toInt != 0) == (segment.haveReflexivePronoun)
					         case _ => { 
					            //println(head)
					            false
					         }
					       }
	      if (matchVal) {
	        compareProperties(segment,attribs.tail)
	        }
	      else {
	       matchVal
	      }
       }
     }
     compareProperties(segment,this.attributes)
    
   }
   
   override def isEmpty : Boolean = false
   override def toString : String =  "SegmentCondition - count cond " + attributes.size
}
