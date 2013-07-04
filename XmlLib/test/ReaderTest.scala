
import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.xml._
import Xml._
import Xml.Reader


@RunWith(classOf[JUnitRunner])
class ReaderTest extends FunSuite {

  test("read nodes") {
  def reader = new Xml.Reader("test.xml")
  def rootNodes = reader.data \\ "sentence"
  reader.getChildNodes(reader.data) foreach (t => println("key:" + t._1 + " \n text:" + t._2))
  reader.getChildNodes(rootNodes.head) foreach (t => println("key:" + t._1 + " \n text:" + t._2))
 // List[Node]{reader.data}.toList.foreach(f => println("key:" + f.label + " text:" + f.text))
  }
 
  test("read attributes") {
  def reader = new Xml.Reader("test.xml")
  def attributes = reader.getAttributes(reader.data)
  attributes foreach (t => println("key:" + t._1 + "text:" + t._2))
  }
  
}