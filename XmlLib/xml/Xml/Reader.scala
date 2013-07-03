package Xml
import scala.xml._
import  java.io._


class Reader(file : String) extends XmlReader {

  val data : Node = ReadFile(new File(file))
  
  def getAttributes(node : Node) : List[(String,String)] = {
    node.attributes.map( t => (t.key,t.value.head.text)).toList
  }
  
  def getChildNodes(node : Node) : List[(String,String)] = {
    
    node.child.filterNot(p => p.isAtom) map (t => (t.label,t.text)) toList
  }
  
  private def getAttribute(node : Node , attrib : String) : String = 
  {
    val value = getAttributes(node).filter(_ == attrib)
    if (value.isEmpty) ""
    else value.head._2
  }
  
 def getTextAttribute(node : Node , attrib : String) : String = this.getAttribute(node, attrib)
  
 def getBooleanAttribute(node : Node, attrib : String) : Boolean = {
   val value = this.getAttribute(node,attrib)
   if (value.isEmpty) false
   else if (value == "1" || value == "true") true
   else false
 }
}