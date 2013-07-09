package DataObjectTests

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import segmenter.Configuration
import scala.xml._
import common._
import ClauseEstimate.RuleHandler
import Rules.SegmentTemplate
import common.segment.PureSegment
import common.segment.Boundary
import common.segment.TaggedSegment
import Rules.RuleAutomata

@RunWith(classOf[JUnitRunner])
class ConfigTest extends FunSuite {
  
   test("read data folder") {
     val dirName = Configuration.DataFolder
     print(dirName)
     assert(dirName == "../GoldenData/Seg")
   }
   
    
  test("rule test") {
      val rule = RuleHandler.rules.head
      
      val words1  =  List( 
      new MorfWord("formp6","P6_____________"),
      new MorfWord("formA","AA_____________"),
      new MorfWord("níž","P9FS6----------")
      )
      
      val conj  =  List( 
      new MorfWord("formJ","J^_____________")
      )
      
       val comma  =  List( 
      new MorfWord(",","Z:_____________")
      )
      
      val words2  =  List( 
      new MorfWord("formVF","Vf_____________"),
      new MorfWord("formvi","VB_____________"),
      new MorfWord("formN","NN_____________"),
      new MorfWord("formA","AA_____________"),
      new MorfWord("formp6","P6_____________"),
      new MorfWord("formA","AA_____________"),
       new MorfWord("níž","P9FS6----------")
      )
      

    
     val segment = new PureSegment(words1,1,false)
        segment.setClause(1)
       val segment2 = new PureSegment(words2,1,false)
        segment2.setClause(2)
     val conjSegment = new Boundary(conj,1)
        conjSegment.setClause(1)
       
     val conjComma = new Boundary(comma,1)
        conjComma.setClause(1)
     val tSegment = new TaggedSegment(segment)
     val tSegment2 = new TaggedSegment(segment2)
     val tBoundary = new TaggedSegment(conjSegment)
     val tBoundary2 = new TaggedSegment(conjComma) 
     val sentence : List[TaggedSegment] = List(tSegment, tBoundary2, tSegment2,tSegment2, tBoundary2, tSegment2)
    
    println(RuleAutomata.ruleMatches(sentence,rule))
    
   
       1
    }
   
  
}