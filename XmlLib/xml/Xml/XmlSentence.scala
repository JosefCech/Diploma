package Xml
import common._
import scala.xml._

class XmlSentence(segments : List[Segment]) extends XmlWritable {

  
  def TransformXml = this.CreateSentenceNode(segments)
  protected def CreateSentenceNode(sentence : List[Segment]) : Node = {
    val segments =  sentence.map(t => CreateSegmentNode(t)).toList
    <root>{segments}</root>
  }
  
  protected def CreateSegmentNode(segment : Segment) : Node = {
  val words = segment.words.map(t => CreateWordNode(t)).toList  
  if (segment.level.toString() != "-1")
  {
    <segment level={ segment.level.toString }  clause= { segment.clause} >{words}</segment>
  }
  else {
	  <segment>{words}</segment>
  }
  }
  
  protected def CreateWordNode(word : Word) : Node = word match {
   case word : MorfWord => <word form={word.form} tag={word.tag}></word>
   case word : Word => <word form={word.form}></word>
  }
}
