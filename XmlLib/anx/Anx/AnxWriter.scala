package Anx

import Xml._
import scala.xml._


object AnxWriter extends XmlWriter {
  
  val Encoding = "UTF-8"
    

  def Write (f: String, node : Node) : Boolean =
  {
   scala.xml.XML.save(f,node)
    true
  }
  
  def Write(f : String, xmlObject : XmlWritable) : Boolean =
  {
    Write(f,xmlObject.TransformXml)
  }
}