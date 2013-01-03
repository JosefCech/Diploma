package DataObjects

import Xml.XmlWritable
import scala.xml._
import common._

class Sentence(val sentence : List[List[Word]]) extends Xml.XmlWritable {

   
  def CreateSentenceNode(sentence : List[List[Word]]) : Node = {
    
    val root : Elem = <sentence></sentence>
    def segmentsNodes = sentence.map(t => CreateSegmentNode(t))
    Elem(null, "sentence", null, xml.TopScope, segmentsNodes : _*)
  }

  def CreateSegmentNode(segment : List[Word]) : Node = {
    
  def wordNodes = segment.map(w => CreateWordNode(w))
     Elem(null, "segment", null, xml.TopScope, wordNodes : _*)
  }
  
  
  def CreateWordNode(w: Word) : Node = w match {
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
  }

  def TransformXml = CreateSentenceNode(this.sentence)
}