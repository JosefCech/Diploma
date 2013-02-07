package Tests

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Main.{Clauser, RuleClauser}
import Rules.{AnalyzedSentence, RuleHandler}
import java.io.{File}
import common.{AnalyzedSegment}

@RunWith(classOf[JUnitRunner])
class RuleClauserTest extends FunSuite {

   test("rule clauser test") {
      val files = Clauser.getFiles("Results").take(20)
      val sentences = files.map(f => (f.getName,Anx.AnxReader.ReadSentence(f)))
  /*   val data = sentences.map(f => {
        println(f._2)
        RuleClauser.analyzeSentence(new Sentence(f._2))
      }
      )*/
   }
   
   test("test sentence") {
     val files = Clauser.getFiles("Results").take(1)
     val sentences = files.map(f => (f.getName,Anx.AnxReader.ReadSentence(f))).toList.map(f => f._2.map(t => new AnalyzedSegment(t)).toList)
     
    // sentences.head.foreach(f => println(f.isBoundarySegment + " " + f.segment.toString))
     var data = RuleHandler.applicableRules(sentences.head.zipWithIndex.toList.map(t => t.swap))
   
      println(RuleClauser.getRuleSegments(data))
      println(RuleClauser.getRuleLevel(data))
   }
   
   test("test rule clauser") {
     
   }
   
}
