package test
import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common.sentence.MorfSentence
import common.MorfWord
import common.segment.{ Segment, Boundary }
import scala.xml._

@RunWith(classOf[JUnitRunner])
class SentenceTest extends FunSuite {
  
  test("get bounderies from sentence") {
     val words =  List( new MorfWord ("a","J^------------") , new MorfWord ("který","J,------------"), new MorfWord ("a","J^------------"))
     val sentence = new MorfSentence(words)
     println(sentence.getBounderies)
     print("boundaries")
     println(sentence.getSubFlags)
     print("subflags")
     0
  }
  
    test("get segments from sentence") {
     val words =  List( new MorfWord ("a","J^------------") , new MorfWord ("ale","J^------------"), new MorfWord ("který","J,------------"), new MorfWord ("a","J^------------"))
     val sentence = new MorfSentence(words)
     sentence.segments.foreach(f => { println(f.toString) ; println("") })
     0
  }
    
  test("get segments from sentence with learn data") {
     val words =  List( new MorfWord ("a","J^------------") , new MorfWord ("ale","J^------------"), new MorfWord ("který","J,------------"), new MorfWord ("a","J^------------"))
     val levels = List((0,0),(1,0),(2,0),(3,0))
     val sentence = new MorfSentence(words)
     sentence.parsedSegments(sentence.morfWords, levels).foreach(f => { print(f.toString + " ")
                                                    println(f.level)})
     0
  }
  
  test("set level of segments") {
     val words =  List( new MorfWord ("rychlost","NNFS1-----A----") , new MorfWord ("(","Z:-------------"),  new MorfWord ("A","NNFXX-----A---8"),new MorfWord (")","Z:-------------"),new MorfWord (":","Z:-------------"))
      val sentence = new MorfSentence(words)
     sentence.segments.map(
         p => { p.setLevel(2)
                p
         }).foreach(p => println(p.level))
  }
  
 /*   test("test estimate clause") {
     val words =  List( new MorfWord ("rychlost","NNFS1-----A----") , new MorfWord ("(","Z:-------------"),  new MorfWord ("A","NNFXX-----A---8"),new MorfWord (")","Z:-------------"),new MorfWord (":","Z:-------------"))
     val sentence = new MorfSentence(words)
     println(sentence.estimationOfClause)
  }
   test("test estimate clause 2 ") {
     val words =  List( new MorfWord ("rychlost","NNFS1-----A----") , new MorfWord (":","Z:-------------"))
     val sentence = new MorfSentence(words)
     println(sentence.estimationOfClause)
  }
  */
}
