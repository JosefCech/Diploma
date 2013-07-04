
import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.xml._
import Xml._
import Anx.RulesReader
import java.io.File
import Rules.Rule


@RunWith(classOf[JUnitRunner])
class RuleReaderTest extends FunSuite {

  test("read nodes") {
  val test : List[Rule] = RulesReader.ReadRules(new File("AutomataRules.xml"))
  println(test)
 // def rootNodes = reader.data \\ "sentence"
 // reader.getChildNodes(reader.data) foreach (t => println("key:" + t._1 + " \n text:" + t._2))
  //reader.getChildNodes(rootNodes.head) foreach (t => println("key:" + t._1 + " \n text:" + t._2))
 // List[Node]{reader.data}.toList.foreach(f => println("key:" + f.label + " text:" + f.text))
  }
 

  
}
