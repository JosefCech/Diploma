package Rules

import scala.xml.{Node}


object RuleHelper {
    
  def parseRule(xml : Node) : RuleClauser = {
     new RuleClauser {
      template = getTemplate((xml \\ "Template").head)
      level = (xml \\ "@Priority").head.text.toInt
      effect = new Effect {
         segment = (xml \\ "Effect" \\ "@Segment").text
         level  = (xml \\ "Effect" \\ "@Level").text
         effectOn = (xml \\ "Effect" \\ "@EffectOn").text
      }
    }
  }
   
  
 private  def getTemplate(template : Node) : List[ConditionBase] = {
    
    def getSegmentCondition(cond : Node) : ConditionBase = {
     def getAttributes(att : List[Node] , acc : List[(String,String)]) : List[(String,String)] = {
        if (att.isEmpty) {
          acc.reverse
          }    
        else {
          getAttributes(att.tail,((att.head \\ "@name").text,(att.head \\ "@values").text) :: acc)          
        }
      }        
     new ConditionSegment {
         this.attributes = getAttributes((cond\\ "Attributes").toList,List[(String,String)]())
      }
    }
      
    def getBoundaryCondition(cond : Node) : ConditionBase = {
    		def getBoundaries(att : List[Node] , acc : List[(String,String)]) : List[(String,String)] = {
	        if (att.isEmpty) { 
	          acc.reverse
	        }
	        else {
	          getBoundaries(att.tail,((att.head \\ "@form").text,(att.head \\ "@tag").text) :: acc)          
	        }
	      }        
      new ConditionBoundary {
         this.boundary = getBoundaries((cond \\ "Boundary").toList, List[(String,String)]())
      }
    } 
    
    def getTemplateSegment(nodes : List[Node], acc : List[ConditionBase]) : List[ConditionBase] = {
      if (nodes.isEmpty) {
        acc.reverse
      }
      else {
      if (nodes.head.label == "#PCDATA") {
        getTemplateSegment(nodes.tail,acc)
      }
      else {
	      val cond : ConditionBase = nodes.head.label match { case "Segment" => getSegmentCondition(nodes.head)
	     												  case "Boundary"=> getBoundaryCondition(nodes.head)
	     												  case _         => throw new Exception("unknown xml condition")
	    										}	                        
	       getTemplateSegment(nodes.tail, (cond) :: acc)
         }
      }
    }
    
    getTemplateSegment(template.child.toList,List[ConditionBase]())
  }
}
