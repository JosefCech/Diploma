package Tests

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common.segment.{TaggedSegment , Boundary, PureSegment}
import common.{MorfWord, RuleWord }
import scala.xml.{Node}
import Rules.{Condition, Rule, SegmentTemplate, Effect }
import Rules.{ RuleHelper , RuleAutomata }

@RunWith(classOf[JUnitRunner])
class  RuleAutomataTest extends FunSuite  {

   test("condition test") {
     val template : List[SegmentTemplate] = List(new SegmentTemplate("S","", List[RuleWord](),false))
     val condition : Condition  = new Condition(template)
     val rule = new Rule(condition,new Effect)
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
     val sentence : List[TaggedSegment] = List(tSegment)
    
    // println(RuleAutomata.conditionMatch(sentence.zipWithIndex,rule))
    val result = RuleAutomata.conditionMatch(sentence.zipWithIndex,rule.condition.segmentTemplate,new SegmentTemplate(),List[(Int, List[TaggedSegment])]())
    assert(!result.isEmpty)
   }
   
   test("rule test") {
        val template : List[SegmentTemplate] = List(new SegmentTemplate("S","", List[RuleWord](),false),
        											new SegmentTemplate("B","", List[RuleWord](),false),
        											new SegmentTemplate("S","", List[RuleWord](),false)
                                               )
     val condition : Condition  = new Condition(template)
     val effect = new Effect("$3","subtract","1")
     val rule = new Rule(condition,effect)
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
        
     val sentence : List[TaggedSegment] = List(tSegment, tBoundary, tSegment2)
    
    println(RuleAutomata.conditionMatch(sentence.zipWithIndex,rule))
   }

}