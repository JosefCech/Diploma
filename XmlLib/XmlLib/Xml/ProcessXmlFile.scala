package Xml
import scala.xml._
import java.io.File

trait XmlReader {
  def ReadFile(f : File) : Node = {
    XML.loadFile(f)
  }
  
   def getAttributes(node : Node) : List[(String,String)] = {
    node.attributes.map( t => (t.key,t.value.head.text)).toList
  }
  
  def getChildNodes(node : Node) : List[(String,String)] = {
    
    node.child.filterNot(p => p.isAtom) map (t => (t.label,t.text)) toList
  }
  
  private def getAttribute(node : Node , attrib : String) : String = 
  {
    val value = getAttributes(node).filter(_._1 == attrib)
    if (value.isEmpty) ""
    else value.head._2
  }
  
 def getChildNodesWithAtrib(node : Node ) : List[(String,List[(String,String)])] = {   // return (elemName,(atrName,atrValue))
   
    node.child.filterNot(p => p.isAtom) map (t => (t.label,getAttributes(t))) toList
 }
 def getTextAttribute(node : Node , attrib : String) : String = this.getAttribute(node, attrib)
  
 def getBooleanAttribute(node : Node, attrib : String) : Boolean = {
   val value = this.getAttribute(node,attrib)
   if (value.isEmpty) false
   else if (value == "1" || value == "true") true
   else false
 }
}

trait XmlWriter {
  def Write (f: String, node : Node) : Boolean
  def Write (f: String, xmlObject : XmlWritable) : Boolean
}

trait XmlWritable {
  def TransformXml : Node  
}

