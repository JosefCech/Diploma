package Rules

import common.AnalyzedSegment
import java.io.File
import common.{ Clause }
import wordProperties.TagMatcher

object RuleHandler extends Xml.XmlReader {
   
  def rules : List[ClauserRule] = (ReadFile(new File("Resource/Rules.xml")) \\ "Rules" \\ "Rule").map(f => RuleHelper.parseRule(f)).toList
   
  def compareSegment(segment : AnalyzedSegment, condition : SegmentCondition) : Boolean = {
    
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
      val matchProp : Boolean = compareProperties(segment,condition.attributes)
      println(matchProp)
      println(segment.segment.ToString)
      matchProp
  }
  
  def compareBoundarySegment(segment : AnalyzedSegment, condition : BoundaryCondition) : Boolean = {
    val formMatch =  {
      if (condition.form.contains("|")) {
         println(condition.form.split('|').toList)
	     condition.form.split('|').map(t => t.trim).toList.contains(segment.boundary.head.form.trim)
	    }
      else if (condition.form.isEmpty) {
         true
       }
      else {
         condition.form == segment.boundary.head.form 
       }
     }
 val tagMatch = {
     if (condition.tag.contains("|")) {
          condition.tag.split("|").foldLeft(false)((comp,compTag) => ( comp || TagMatcher.Match(segment.boundary.head.tag, compTag))) 
	    }
	  else if (condition.tag.isEmpty){
	    true
	  }
	  else {
	    TagMatcher.Match(segment.boundary.head.tag, condition.tag)
      }
    }  
    
 println("boundary_compare" +  ((formMatch  ) && (tagMatch )) + " " + segment.segment.toString + " " + formMatch + " " + tagMatch + " " + condition.form + " " + segment.boundary.head.form )
     (formMatch  ) && (tagMatch )
  }
  
  def compareBaseSegment(segment : AnalyzedSegment, condition : Condition) : Boolean  = condition match {
    case condition : SegmentCondition => 
      {  if (segment.isBoundarySegment) {
          false
         }
        else {
          compareSegment(segment,condition)
        }
      }
    case condition : BoundaryCondition =>
      {
       if (!segment.isBoundarySegment) {
         false
       }
       else {
         compareBoundarySegment(segment,condition)
       }
     }
}
  
  def compareTemplate(segments : List[(Int,AnalyzedSegment)], template : List[Condition], firstCondition : Condition , nextIndex : Int) : (Int,Boolean, Boolean) = {
     if (segments.isEmpty && template.isEmpty) {
       (nextIndex,true,true)
     }
     else if (segments.isEmpty) {
       
       (nextIndex,false,true)
     }
     else if (template.isEmpty) {
       
       (nextIndex,true,false)
     }
     else {
       val head = segments.head
      
       val matchCondition =  compareBaseSegment(head._2,template.head)
          
       val setFirstCondition = 
           if (firstCondition.isEmpty) {
             template.head
           }
           else {
             firstCondition
           }
           
       val setIndex = {
        if (firstCondition.isEmpty) {
          nextIndex
        }
        else if (nextIndex == 0 && compareBaseSegment(head._2,firstCondition)) {
          head._1
        }
        else {
          nextIndex
        }
       }
      
      if (matchCondition) {
        compareTemplate(segments.tail,template.tail,setFirstCondition,setIndex)
      }
      else {
          (nextIndex,false,segments.size <= template.size)
      }
     }   
  }

  def applicableRule(segments : List[(Int,AnalyzedSegment)], rule : ClauserRule) : List[(Int,Effect,Int)] = {
  
  def applicableRuleAcc(segments : List[(Int,AnalyzedSegment)], template : List[Condition],acc : List[Int]) : List[Int] = {
     if (segments.isEmpty) { 
       acc
     }
     else {
       val result = compareTemplate(segments,template,new Condition(),0)
       val updateAcc =  
         if (result._2) {
           segments.head._1 :: acc
          }
         else {
           acc
         }
       val segmentsAcc = 
          if (result._2) {
            segments.drop(template.size)
          }
          else if (result._1 == 0 ) {
            segments.tail
          }
          else {
            segments.filterNot(p => p._1 < result._1).toList
          }
       applicableRuleAcc(segmentsAcc,template,updateAcc)
     }
   }
     val template = rule.template
     val indexes = applicableRuleAcc(segments,template, List[Int]())
     
     indexes.map(s => (s, rule.effect, rule.level)).toList
     
   }

 def applicableRules(sentence : List[(Int,AnalyzedSegment)], rules : List[ClauserRule]) : List[(Int,Effect,Int)] = {
     rules.map(t => applicableRule(sentence,t)).flatten
  }
  
 def applicableRules(sentence : List[(Int,AnalyzedSegment)]) :  List[(Int,Effect,Int)] = {
   
  this.applicableRules(sentence, rules) 
}
   }
