package test
import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common.sentence.AnxSentence
import common.MorfWord
import common.segment.{ Segment, BaseSegment  }
import scala.xml._


@RunWith(classOf[JUnitRunner])
class AnxSentenceTest extends FunSuite {
  
	 test("test anx sentence") {
	 val words =  List( new MorfWord ("a","J^------------") , new MorfWord ("který","J,------------"), new MorfWord ("a","J^------------"))
	 val sentence = new AnxSentence( List[Segment](new BaseSegment(words)))
	 println(sentence.Segments.toString)   
	 0
	 }
  
	test("compare test anx sentence") {
	 val words =  List( new MorfWord ("a","J^------------") , new MorfWord ("který","J,------------"), new MorfWord ("a","J^------------"))
	 val sentence = new AnxSentence( List[Segment](new BaseSegment(words)))
	 val sentence2 = new AnxSentence( List[Segment](new BaseSegment(words)))
	 sentence.compareWords(sentence2.Words)
	}

	 test("compare test anx sentence 2") {
	 val words =  List( new MorfWord ("a","J^------------") , new MorfWord ("který","J,------------"), new MorfWord ("a","J^------------"))
	 val words2 =  List( new MorfWord ("a","J^------------") , new MorfWord ("který","J,------------"))     
     val sentence = new AnxSentence( List[Segment](new BaseSegment(words)))
     val sentence2 = new AnxSentence( List[Segment](new BaseSegment(words2)))
     sentence!=sentence2
	 }
	  
 
 

}
