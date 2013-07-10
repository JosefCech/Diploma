package Rules

/**
 *  class Effect 
 *    
 **/

 class Effect(val effectOn : String,val effectType : String ,val level : String)
 {
  def this() = this("","","")
  
  def isEmpty : Boolean = effectOn.isEmpty && effectType.isEmpty && level.isEmpty
  
  override def toString : String = "Effect : " + effectOn.toString + " " + effectType.toString + " " + level.toString
 }

class MatchEffect(val effectOnIndex : Int, val levelNum : Int)
 {
  override def toString : String = "Effect : " + effectOnIndex.toString + " " + levelNum.toString
  
  def isEmpty : Boolean = effectOnIndex == -1
 }
