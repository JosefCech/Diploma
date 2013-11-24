package Xml

import scala.xml._
import  java.io._

object Writer extends XmlWriter {
  
  override def Write(f : String, root : Node) : Boolean = {
    val file = new File(f);
    if (!file.exists)
    {
      file.createNewFile
    } 
    
    if (file.canWrite)
    {
     scala.xml.XML.save(f,root,"UTF-8")
    }
    else {
      print("Nelze zapisovat do souboru :" + f)
    }
     true
  }
  
  override  def Write (f: String, xmlObject : XmlWritable) : Boolean = {
    Write(f,xmlObject.TransformXml)
  }

}