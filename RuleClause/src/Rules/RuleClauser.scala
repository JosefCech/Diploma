package Rules
/**
 *	Rules
 *   - condition for application
 *   		- template for using segments
 *   		- each segment in template has description - setting values of options
 *   - level (of  effecient)
 *   - effect - create new clause on same segment
 *            - create new clause on  level below
 *            - create new clause on level above
 *            - add to clause on same level
 *            - add to clause on level above
 *            - add to clause on level below (direct speech)
 *
 **/




 class RuleClauser extends Rule {

  var template : List[ConditionBase] = List[ConditionBase]()
  var level : Int = -1
  var effect : Effect = new Effect()
  override def toString : String =
    { "Clauser Rule \n " +  template.map(t => t.toString).toList.foldLeft("")((acc,f) => acc + "\n" + f ) + "\n"  + "Level :" + level + "\n" +  effect.toString
    }
}
