package test
import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.xml._

@RunWith(classOf[JUnitRunner])
class SentenceTest extends FunSuite {
  
  test("get bounderies from sentence") {
     val words =  List( new MorfWord ("a","J^------------") , new MorfWord ("který","J,------------"), new MorfWord ("a","J^------------"))
     val sentence = new Sentence(words)
     println(sentence.GetBounderies)
     print("boundaries")
     println(sentence.GetSubFlags)
     print("subflags")
     0
  }
  
    test("get segments from sentence") {
     val words =  List( new MorfWord ("a","J^------------") , new MorfWord ("ale","J^------------"), new MorfWord ("který","J,------------"), new MorfWord ("a","J^------------"))
     val sentence = new Sentence(words)
     sentence.segments.foreach(f => { println(f.ToString) ; println("") })
     0
  }
    
  test("get segments from sentence with learn data") {
     val words =  List( new MorfWord ("a","J^------------") , new MorfWord ("ale","J^------------"), new MorfWord ("který","J,------------"), new MorfWord ("a","J^------------"))
     val levels = List((0,0),(1,0),(2,0),(3,0))
     val sentence = new Sentence(words)
     sentence.parsedSegments(levels).foreach(f => { print(f.ToString)
                                                    println(f.level)})
     0
  }

}