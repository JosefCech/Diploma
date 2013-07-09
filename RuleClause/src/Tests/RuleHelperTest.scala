package Tests

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common.segment.{TaggedSegment , Boundary, PureSegment}
import common.{MorfWord, RuleWord }
import scala.xml.{Node}
import Rules.{Condition, Rule, SegmentTemplate, Effect }
import Rules.RuleHelper
import Rules.MatchEffect

@RunWith(classOf[JUnitRunner])
class RuleHelperTest extends FunSuite  {
 
  test("compare template segment") {
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
  
    test("compare template segment words") {
    val template1 =  new SegmentTemplate("B","", List(new RuleWord(",","Z:")),false)
    
    
    val words  =  List( 
      new MorfWord("formVF","Vf_____________"),
      new MorfWord("formvi","Vi_____________"),
      new MorfWord("formN","NN_____________"),
      new MorfWord("formA","AA_____________"),
      new MorfWord("formp6","P6_____________"),
      new MorfWord("formA","AA_____________"),
       new MorfWord("níž","P9FS6----------")
      ) 
    val words2  =  List( 
      new MorfWord(",","Z:_____________")
      ) 
      
    val segment = new PureSegment(words,1,false) 
    val tSegment = new TaggedSegment(segment)
     val segment2 = new Boundary(words2,1) 
    val tSegment2 = new TaggedSegment(segment2)
    assert(!RuleHelper.compareSegmentTemplate(tSegment, template1))
   assert(RuleHelper.compareSegmentTemplate(tSegment2, template1))

  }
    
      test("apply rule") {
             val words  =  List( 
      new MorfWord("formVF","Vf_____________"),
      new MorfWord("formvi","Vi_____________"),
      new MorfWord("formN","NN_____________"),
      new MorfWord("formA","AA_____________"),
      new MorfWord("formp6","P6_____________"),
      new MorfWord("formA","AA_____________"),
       new MorfWord("níž","P9FS6----------")
      )
    
      val conj  =  List( 
      new MorfWord("formJ","J^_____________")
      ) 
    
     val segment = new PureSegment(words,1,false)
     segment.setClause(1)
     val segment2 = new PureSegment(words,1,false)
     segment2.setClause(2)
     val conjSegment = new Boundary(conj,1)
     conjSegment.setClause(1)
     val tSegment = new TaggedSegment(segment)
     val tSegment2 = new TaggedSegment(segment2)
     val tBoundary = new TaggedSegment(conjSegment)
        
     val sentence = List(segment,conjSegment,segment2)
     var rules = List(new MatchEffect(2,1))
     var updated = RuleHelper.applyRule(rules, sentence)
     assert(sentence.apply(2).clause == 1)
     rules = List(new MatchEffect(2,1),new MatchEffect(1,2),new MatchEffect(2,2))
     updated = RuleHelper.applyRule(rules, sentence)
     assert(sentence.apply(1).clause == 2)
     assert(sentence.apply(2).clause == 2)
     
    }
}
