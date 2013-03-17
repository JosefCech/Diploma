package Rules

import common.AnalyzedSegment
import java.io.File
import common.{ Clause }
import wordProperties.TagMatcher

object RuleHandler extends Xml.XmlReader {
   
  // TODO get rules.xml file from configuration
  def rules : List[RuleClauser] = (ReadFile(new File("Resource/Rules.xml")) \\ "Rules" \\ "Rule").map(f => RuleHelper.parseRule(f)).toList
   

  def compareBaseSegment(segment : AnalyzedSegment, condition : ConditionBase) : Boolean  = condition match {
    case condition : ConditionSegment => 
      {  if (segment.IsBoundarySegment) {
           false
         }
         else {
           condition.satisfiesSegment(segment)
         }
      }
    case condition : ConditionBoundary =>
      {
       if (!segment.IsBoundarySegment) {
         false
       }
       else {
          condition.satisfiesSegment(segment)
       }
     }
}
  
 
  /**
   * Return index of segments, where starts application of rule in sentence 
   *
   **/
  def applicableRule(segments : List[(Int,AnalyzedSegment)], rule : RuleClauser) : List[(Int,Effect,Int)] = {
  
	def tableFind(
      segments : List[(Int,AnalyzedSegment)], 
      template : List[ConditionBase],
      tableActual : List[(Int,Int)]) : List[(Int,Int)] = {
	  if (segments.isEmpty)
	    {
	      tableActual.filter(p => p._2 == template.size)
	    }
	    else {
	      val compareConditions  = tableActual.map(t => (t._1 + 1, t._2))
	      val defaultValue : (Int,Int) =  (-1,-1) 
	      val tableNew : List[(Int,Int)] = (defaultValue :: compareConditions.map( t => {
		      if (t._1 > template.size)
		      {
		         ((t._1-1),t._2)
		      }
		      else if (compareBaseSegment(segments.head._2, template.apply(t._1)))
		      {
		        (t._1,segments.head._1) 
		      }
		      else {
		        (-1,-1)
		      }
	      }
	     )).distinct
	    tableFind(segments.tail, template, tableNew)
	    }
	  }
	  
     val template = rule.template
     val indexes = tableFind(segments,template, List[(Int,Int)]((-1,-1)))
     
     indexes.filter(t => t._1 == template.size).map(s => (s._2, rule.effect, rule.level)).toList
     
   }

 def applicableRules(sentence : List[(Int,AnalyzedSegment)], rules : List[RuleClauser]) : List[(Int,Effect,Int)] = {
     rules.map(t => applicableRule(sentence,t)).flatten
  }
  
 def applicableRules(sentence : List[(Int,AnalyzedSegment)]) :  List[(Int,Effect,Int)] = {
   
  this.applicableRules(sentence, RuleHandler.rules) 
}
   }
