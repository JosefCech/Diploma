package DataObjects
import Xml.XmlWritable
import scala.xml._
import common.{MorfWord, Word, AnalyzedWord }
import common.segment.{ AnalyzedSegment, Segment }
import scala.xml.{ MetaData ,UnprefixedAttribute}
/**
 * class Sentence implements Xml.XmlWritable 
 **/
class BaseXmlSentence(val sentence : Any) extends Xml.XmlWritable {

   
  def CreateSentenceNode(sentence : Any) : Node =  {
   
    val root : Elem = <sentence></sentence>
   
    def segmentsNodes = sentence match {
      case sentence : List[Any] => sentence.map(t => CreateSegmentNode(t))
    }
      
     
    Elem(null, "sentence", null, xml.TopScope, segmentsNodes : _*)
  }

  def CreateSegmentNode(segment : Any) : Node = {
             
  def wordData : (MetaData, List[Node]) = segment match {
  
    case segment : AnalyzedSegment => { 
    									var attributes = new UnprefixedAttribute("isStartClause",segment.getStartNewClause.toString,null)
    									attributes.append(new UnprefixedAttribute("level",segment.level.toString,null))
                                        (attributes,segment.words.map(w => CreateWordNode(w)))
                                      }
    case segment : List[Segment] => (null,segment.map(w => CreateWordNode(w)))
  }
     Elem(null, "segment", wordData._1, xml.TopScope, wordData._2 : _*)
     
  }
  
  
  def CreateWordNode(w: Any) : Node = w match {
    case w : MorfWord =>	{ 
    							val form = new Some(w.form)
    							val tag = new Some(w.tag)
                           		val word : Node = <word form={form.get} tag={tag.get}></word> 
                           		word	
    					 	}
    case w : Word =>	{
    						val form = new Some(w.form)
    						val word : Node = <word form={form.get}></word>
    						word
    				 	}
    case w : AnalyzedWord => {
                              val form = new Some(w.word.form)
                              val tag = new Some(w.word.tag)
                              val separator = new Some(w.separator.toString)
                              val clauseNum = new Some(w.clauseNum.toString)
                              val word : Node = <word form={form.get} tag={tag.get} separator={separator.get} clauseNum={clauseNum.get}></word>
                              word
                           }
  }

  def TransformXml = CreateSentenceNode(this.sentence)
}


