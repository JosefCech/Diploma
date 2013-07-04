package Tests

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common.segment.{TaggedSegment , Boundary, PureSegment}
import common.{MorfWord, RuleWord }
import scala.xml.{Node}
import Rules.{Condition, Rule, SegmentTemplate, Effect }
import Rules.RuleHelper

@RunWith(classOf[JUnitRunner])
class RuleHelperTest extends FunSuite  {
 
  test("compare tempalte segment") {
    val template1 =  new SegmentTemplate("S","", List[RuleWord](),false)
     val template2 =  new SegmentTemplate("__C","", List[RuleWord](),false)
     val template3 =  new SegmentTemplate("S_C","", List[RuleWord](),false)
     val template4 =  new SegmentTemplate("S__X9X","", List[RuleWord](),false)
    
    val words  =  List( 
      new MorfWord("formVF","Vf_____________"),
      new MorfWord("formvi","Vi_____________"),
      new MorfWord("formN","NN_____________"),
      new MorfWord("formA","AA_____________"),
      new MorfWord("formp6","P6_____________"),
      new MorfWord("formA","AA_____________"),
       new MorfWord("níž","P9FS6----------")
      ) 
    val segment = new PureSegment(words,1,false) 
    val tSegment = new TaggedSegment(segment)
    assert(RuleHelper.compareSegmentTemplate(tSegment, template1))
    assert(RuleHelper.compareSegmentTemplate(tSegment, template2))
    assert(RuleHelper.compareSegmentTemplate(tSegment, template3))
    assert(!RuleHelper.compareSegmentTemplate(tSegment, template4))
  }
}
