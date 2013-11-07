import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import Pdt._
import scala.xml._
import java.io._

@RunWith(classOf[JUnitRunner])
class MorfReaderTest extends FunSuite {
  
  test("morf reader") {
   def data =  MorfReader.Read(new File("cmpr9406_001.m"))
   println(data.size)
   0
 }
}