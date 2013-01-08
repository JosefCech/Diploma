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
     val words =  List( new MorfWord ("a","J^------------") , new MorfWord ("který","J,------------"), new MorfWord ("a","J^------------"))
     val sentence = new Sentence(words)
     sentence.parsedSegments.foreach(f => { f.words.foreach(w =>  {print(w.form) ; print(" ")}) ; println("") })
     0
  }

}