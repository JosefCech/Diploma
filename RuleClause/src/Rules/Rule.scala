package Rules

/**
 *   Rule should represented single different rule in each instance
 *    
 **/

class Rule( val condition: Condition, val effect : Effect )
{
  def isEmptyCondition : Boolean = condition.isEmpty && effect.isEmpty
  
  def numOfConditionObject : Int = {
                               if (this.isEmptyCondition){
                                 0
                               }
                                else {
                                  this.condition.segmentTemplate.length
                                }
                             }
  
  override def toString : String = condition.toString + "\n" + effect.toString
}
