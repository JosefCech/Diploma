package Xml
import scala.xml._
import  java.io._


class Reader(file : String) extends XmlReader {

  val data : Node = ReadFile(new File(file))
  
  def GetAttributes(node : Node) : List[(String,String)] = {
    node.attributes.map( t => (t.key,t.value.head.text)).toList
  }
  
  def GetChildNodes(node : Node) : List[(String,String)] = {
    
    node.child.filterNot(p => p.isAtom) map (t => (t.label,t.text)) toList
  }
  
 
  
}