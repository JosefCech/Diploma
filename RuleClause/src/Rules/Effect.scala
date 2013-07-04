package Rules

/**
 *  class Effect 
 *    
 **/

 class Effect(val effectOn : String,val effectType : String ,val clause : String)
 {
  def this() = this("","","")
  
  def isEmpty : Boolean = effectOn.isEmpty && effectType.isEmpty && clause.isEmpty
  
  override def toString : String = "Effect : " + effectOn.toString + " " + effectType.toString + " " + clause.toString
 }

class MatchEffect(val effectOnIndex : Int, val clauseNum : Int)
 {
  override def toString : String = "Effect : " + effectOnIndex.toString + " " + clauseNum.toString
  
  def isEmpty : Boolean = effectOnIndex == -1
 }
