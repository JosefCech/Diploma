import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.xml._
import Xml._
import Xml.Reader

@RunWith(classOf[JUnitRunner])
class WriterTest extends FunSuite {
 
  test("write nodes") {
   val node : Node = <node>test</node>
   val rootNode : Node = <root>{node}</root>
   Xml.Writer.Write("test.xml", rootNode)
  }
  
}