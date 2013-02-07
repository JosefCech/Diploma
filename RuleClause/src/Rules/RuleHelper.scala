package Rules

import scala.xml.{Node}


object RuleHelper {
    
  def parseRule(xml : Node) : ClauserRule = {
     new ClauserRule {
      template = getTemplate((xml \\ "Template").head)
      level = (xml \\ "@Priority").head.text.toInt
      effect = new Effect {
         segment = (xml \\ "Effect" \\ "@Segment").text
         level  = (xml \\ "Effect" \\ "@Level").text
         effectOn = (xml \\ "Effect" \\ "@EffectOn").text
      }
    }
  }
   
  
  private  def getTemplate(template : Node) : List[Condition] = {
    
    def getSegmentCondition(cond : Node) : Condition = {
      def attributeNodes : List[Node] = (cond\\ "Attributes").toList
      
      def getAttributes(att : List[Node] , acc : List[(String,String)]) : List[(String,String)] = {
        if (att.isEmpty) { 
            acc.reverse
        }
        else {
          val attName = (att.head \\ "@name").text
          val attValues = (att.head \\ "@values").text
          getAttributes(att.tail,(attName,attValues) :: acc)          
        }
      }
         
     new SegmentCondition {
         this.attributes = getAttributes(attributeNodes,List[(String,String)]())
      }
    }
    
    
    def getBoundaryCondition(cond : Node) : Condition = {
      val formNew = (cond \\ "@Form").text
      val tagNew = (cond \\ "@Tag").text
      new BoundaryCondition {
         this.form = formNew
         this.tag = tagNew
      }
    } 
    
    def getTemplateSegment(nodes : List[Node], acc : List[Condition]) : List[Condition] = {
      if (nodes.isEmpty) {
        acc.reverse
      }
      else {
      if (nodes.head.label == "#PCDATA") {
        getTemplateSegment(nodes.tail,acc)
      }
      else {
	      val cond : Condition = nodes.head.label match { case "Segment" => getSegmentCondition(nodes.head)
	     												  case "Boundary"=> getBoundaryCondition(nodes.head)
	     												  case _         => {	
	     													  					throw new Exception("unknown xml condition")
	    										}	                        }
	       getTemplateSegment(nodes.tail, (cond) :: acc)
         }
      }
    }
    getTemplateSegment(template.child.toList,List[Condition]())
  }
}
