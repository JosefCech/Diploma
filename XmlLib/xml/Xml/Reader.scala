package Xml
import scala.xml._
import  java.io._


class Reader(file : String) extends XmlReader {

  val data : Node = ReadFile(new File(file))
  
 
}