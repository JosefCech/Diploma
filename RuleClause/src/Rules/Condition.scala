package Rules
import common.AnalyzedSegment
/**
 *  trait  Rule should represented single different rule in each instance
 *    
 **/

 trait Condition  {
  
}

class ConditionBase extends Condition {
  def isEmpty:Boolean = true
  def satisfiesSegment(segment : AnalyzedSegment) : Boolean = {
    false
  }
}



