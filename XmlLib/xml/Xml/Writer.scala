package Xml

import scala.xml._
import  java.io._

object Writer extends XmlWriter {
  
  override def Write(f : String, root : Node) : Boolean = {
     scala.xml.XML.save(f,root)
    true
  }
  
  override  def Write (f: String, xmlObject : XmlWritable) : Boolean = {
    Write(f,xmlObject.TransformXml)
  }

}