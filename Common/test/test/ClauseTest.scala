package test
import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common.sentence.{AnxSentence, MorfSentence}
import common.MorfWord
import common.segment.{ Segment, BaseSegment  }
import scala.xml._
import common.clause.Clause


@RunWith(classOf[JUnitRunner])
class ClauseTest extends FunSuite {

  test("base clause") {
    
    val clause = new Clause(List[(Int,Segment)](),true)
   
    println(clause)
    assert(clause.isOpen)
    assert(!clause.havePreviousClause)
    assert(!clause.haveSuperiorClause)
  }
  
  
}