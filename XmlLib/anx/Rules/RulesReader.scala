package Anx

 import Xml.XmlReader
 import xml.Node
 import java.io.File 
 import common.RuleWord
 import Rules._
 
 
object RulesReader extends XmlReader {

  def ReadRules(file : File) : List[Rule] = {
    val root = this.ReadFile(file);
    val rules = (root \\ "Rule").map( p => CreateRule(p)).toList
    rules
  }
 
  private def CreateRule( ruleXml : Node) : Rule = 
  {
    val condition  = CreateCondition((ruleXml \\ "Condition").head)
    val effect = CreateEffect((ruleXml \\ "Effect").head)
    println(effect)
    val rule = new Rule(condition,effect);
    rule
  }
  
  
  private def CreateCondition(conditionXml : Node) : Condition =
  {
    val templates : List[SegmentTemplate] =  (conditionXml \\ "Segment").map(p => CreateSegmentTemplate(p)).toList
    new Condition(templates)
  }
  
  private def CreateSegmentTemplate(segmentNode : Node) : SegmentTemplate =
  {
    val tags = this.getTextAttribute(segmentNode, "Tag")
    val typeSegment = this.getTextAttribute(segmentNode, "Type")
    val isGroup  = this.getBooleanAttribute(segmentNode,"IsGroupped")
    val level = this.getTextAttribute(segmentNode,"Level")
    val words = (segmentNode \ "Words" \ "Word").toList.map(t => createWord(t)).toList
 
    new SegmentTemplate(tags,typeSegment,words,isGroup)
  }
  
  private def CreateEffect(effectXml : Node) : Effect =
  {
    
    val effectOn = this.getTextAttribute(effectXml,"EffectOn")
    val effectType = this.getTextAttribute(effectXml, "EffectType")
    val clauseNum = this.getTextAttribute(effectXml,"Clause")
    val effect = new Effect(effectOn,effectType,clauseNum)
    effect
  }
  
  private def createWord(wordXml : Node) : RuleWord = 
  {
    val form = this.getTextAttribute(wordXml,"Form")
    val tags = this.getTextAttribute(wordXml,"Tag")
    
    new RuleWord(form,tags)
  }
}