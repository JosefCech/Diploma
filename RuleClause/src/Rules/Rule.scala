package Rules
/*
 * Pravidla se skl�daj� :
 *  - podm�nky (�ablona soustavy segmen� nebo celkov� pohled na v�tu)
 *       - vlastnosti segmentu - dle funkc� v analzyedSegment
 *  - u�innost  - po�ad� v jak�m m� b�t aplikov�na
 *   effekt - nov� segment
 *            p�ipojen� k aktu�ln�mu segmentu
 *
 * */
class Rule {
  def  useCondition : Boolean = false
  }

 class Condition  {
  def isEmpty:Boolean = true
}

 class SegmentCondition extends Condition {
   var attributes : List[(String,String)] = List[(String,String)]()

   override def isEmpty : Boolean = false
   override def toString : String =  "SegmentCondition - count cond " + attributes.size
}

 class BoundaryCondition extends Condition {
  var form : String = ""
  var tag : String = ""

  override def isEmpty : Boolean = false
  override def toString : String =  "Boundary form : " + this.form + ", tag : " + this.tag
 }

 class Effect {
 var segment : String = ""
 var level : String = ""
 var effectOn : String = ""

 override def toString : String = "Effect  behaviour segment: " + segment + "  level " + level + " effect rule" + effectOn
}


 class ClauserRule extends Rule {

  var template : List[Condition] = List[Condition]()
  var level : Int = -1
  var effect : Effect = new Effect()
  override def toString : String =
    { "Clauser Rule \n " +  template.map(t => t.toString).toList.foldLeft("")((acc,f) => acc + "\n" + f ) + "\n"  + "Level :" + level + "\n" +  effect.toString
    }
}
