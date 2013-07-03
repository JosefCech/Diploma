package Rules

 import Xml.XmlReader
 import xml.Node
 import java.io.File 
 import common.RuleWord
 
class RulesReader extends XmlReader {

  def ReadRules(file : File) : List[Rule] = {
    val root = this.ReadFile(file);
    val rules = (root \ "Rule").map( p => CreateRule(p)).toList
    rules
  }
 
  private def CreateRule( ruleXml : Node) : Rule = 
  {
    val condition  = CreateCondition((ruleXml / "Condition").head)
    val effect = CreateEffect((ruleXml / "Effect").head)
    val rule = new Rule(condition,effect);
  }
  
  
  private def CreateCondition(conditionXml : Node) : Condition
  {
    val templates =  (conditionXml / "Segment").map(p => CreateSegmentTemplate(p)).toList
    new Condition(templates)
  }
  
  private def CreateSegmentTemplate(segmentNode : Node) : SegmentTemplate =
  {
    val tags = this.getTextAttribute(segmentNode, "tag")
    val typeSegment = this.getTextAttribute(segmentNode, "type")
    val isGroup  = this.getBooleanAttribute(segmentNode,"isGroup")
    val level = this.getTextAttribute(segmentNode,"level")
    val words = (segmentNode \ "Words" \ "Word").toList.map(t => CreateWord(t)).toList
 
    new SegmentTemplate(tags,typeSegment,isGroup,words)
  }
  
  private def CreateEffect(effectXml : Node) : Effect =
  {
    val effectOn = this.getTextAttribute(effectXml,"effectOn")
    val effectType = this.getTextAttribute(effectXml, "effectType")
    val clauseNum = this.getTextAttribute(effectXml,"clause")
    
    new Effect(effectOn,effectType,clauseNum)
  }
  
  private def createWord(wordXml : Node) : RuleWord = 
  {
    val form = this.getTextAttribute(wordXml,"form")
    val tags = this.getTextAttribute(wordXml,"tag")
    
    new RuleWord(form,tags)
  }
}