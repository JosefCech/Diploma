package DataObjectTests

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Segmenter._
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
   
   test("separators word"){
    Separators.separatorsWords.foreach(f => f match {
      													case f : MorfWord => println(f.form,f.tag) 
      													case f : Word => print(f.form)
              
    												}
    									)
     Separators.subFlags.foreach(f => f match {
      													case f : MorfWord => println(f.form,f.tag) 
      													case f : Word => print(f.form)
              
    												}
    									)
    assert(Separators.separatorsWords.toList.size == 2)
    assert(Separators.subFlags.size == 1)
   }

}