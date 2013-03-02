package DataObjectTests

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import segmenter.Configuration
import scala.xml._
import common._

@RunWith(classOf[JUnitRunner])
class ConfigTest extends FunSuite {
  
   test("read data folder") {
     val dirName = Configuration.DataFolder
     assert(dirName == "Data")
   }
   
    test("data files"){
       
    }
   
  
}