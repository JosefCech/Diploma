package Xml
import common.segment.{Segment, AnalyzedSegment }
import common.{MorfWord, Word, AnalyzedWord, AbstractWord, ClauseInfo, SegmentInfo}
import scala.xml._

class XmlSentence(val segments : List[Segment] ) extends XmlWritable {

  def TransformXml = this.CreateSentenceNode(segments)
  protected def CreateSentenceNode(sentence : List[Segment]) : Node = {
    val segments =  sentence.map(t => {  CreateSegmentNode(t)
    								}).toList
    									
    <root>{segments}</root>
  }
  
  protected def CreateSegmentNode(segment : Segment) : Node = {
  val words = segment.words.map(t => CreateWordNode(t)).toList
  def CreateSegmentNodeData(segment : Segment, words : List[Node]) : Node = segment match {
    case segment : AnalyzedSegment => {
      
           <segment level={segment.Level.toString }  clause={segment.ClauseNum.toString }  clausebeg={segment.Clausebeg.toString}>{words}</segment>
       }
    case _ =>  <segment>{words}</segment>
    
  }
  
   val segmentNode =  CreateSegmentNodeData(segment,words)
   segmentNode
  }
  
  protected def CreateWordNode(word : AbstractWord) : Node = word match {
   case word : AnalyzedWord =>  <word form={word.word.form} tag={word.word.tag}></word>
   case word : MorfWord => <word form={word.form} tag={word.tag}></word>
   case word : Word => <word form={word.form}></word>
  }
}
