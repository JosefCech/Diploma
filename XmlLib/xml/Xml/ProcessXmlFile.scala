package Xml
import scala.xml._
import java.io.File

trait XmlReader {
  def ReadFile(f : File) : Node = {
    XML.loadFile(f)
  }
}

trait XmlWriter {
  def Write (f: String, node : Node) : Boolean
  def Write (f: String, xmlObject : XmlWritable) : Boolean
}

trait XmlWritable {
  def TransformXml : Node  
}

