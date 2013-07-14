package test
import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common.sentence.{AnxSentence, MorfSentence}
import common.MorfWord
import common.segment.{ Segment, BaseSegment  }
import scala.xml._
import common.segment.PureSegment
import common.segment.Boundary


@RunWith(classOf[JUnitRunner])
class AnxSentenceTest extends FunSuite {
  
	 test("test anx sentence") {
	 val words =  List( new MorfWord ("a","J^------------") , new MorfWord ("který","J,------------"), new MorfWord ("a","J^------------"))
	 val sentence = new AnxSentence( List[Segment](new BaseSegment(words)))
	 println(sentence.segments.toString)   
	 0
	 }
  
	test("compare test anx sentence") {
	 val words =  List( new MorfWord ("a","J^------------") , new MorfWord ("který","J,------------"), new MorfWord ("a","J^------------"))
	 val sentence = new AnxSentence( List[Segment](new BaseSegment(words)))
	 val sentence2 = new AnxSentence( List[Segment](new BaseSegment(words)))
	 assert(true, sentence.compareWords(sentence2.Words))
	}

	 test("compare test anx sentence 2") {
	 val words =  List( new MorfWord ("a","J^------------") , new MorfWord ("který","J,------------"), new MorfWord ("a","J^------------"))
	 val words2 =  List( new MorfWord ("a","J^------------") , new MorfWord ("který","J,------------"))     
     val sentence = new AnxSentence( List[Segment](new BaseSegment(words)))
     val sentence2 = new AnxSentence( List[Segment](new BaseSegment(words2)))
     assert(sentence!=sentence2)
     }
	  
	test("compare test anx and morf sentences") {
	 val words =  List( new MorfWord ("a","J^------------") , new MorfWord ("který","J,------------"), new MorfWord ("a","J^------------"))
	 val words2 =  List( new MorfWord ("a","J^------------") , new MorfWord ("který","J,------------"))     
     val sentence = new AnxSentence( List[Segment](new BaseSegment(words)))
	 val sentence2 = new MorfSentence(words2)
     assert(sentence!=sentence2)
	 }
 
     test("compare test anx and morf sentences 2") {
	 val words =  List( new MorfWord ("a","J^------------") , new MorfWord ("který","J,------------"), new MorfWord ("a","J^------------"))
	  val sentence = new AnxSentence( List[Segment](new BaseSegment(words),new BaseSegment(words)))
	 val sentence2 = new MorfSentence(words ::: words)
     assert(sentence==sentence2)
	 } 
     
    test("sentence with standart segment") {
	 val words =  List( new MorfWord ("a","J^------------") , new MorfWord ("který","J,------------"), new MorfWord ("a","J^------------"))
	 val words2 =  List( new MorfWord ("a","J^------------") , new MorfWord ("který","J,------------"))     
     val segment = new PureSegment(words,0)
	 segment.setClause(1)
	 val boundary = new Boundary(words,0)
	 boundary.setClause(0)
     val sentence2 = new AnxSentence( List[Segment](segment,boundary))
     println(sentence2.getTagsWithClause)
     }

}
