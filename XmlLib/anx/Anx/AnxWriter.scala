package Anx

import Xml._
import scala.xml._
import common._
import java.io.File


object AnxWriter extends XmlWriter {
  
  val Encoding = "UTF-8"
    
  def  Write(f : String , sentence : List[Segment]) : Boolean = {
     Write(f,new Xml.XmlSentence(sentence))
  }

  def Write (f: String, node : Node) : Boolean =
  {
    val file = new File(f);
   scala.xml.XML.save(f,node,"UTF-8")
    true
  }
  
  def Write(f : String, xmlObject : XmlWritable) : Boolean =
  {
    Write(f,xmlObject.TransformXml)
  }
}