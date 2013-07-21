package test

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.xml._
import statistic.LevelSegmentModel
import statistic.Models.ConditionalClauseModel



  class ConditionalClauseModelTest extends ConditionalClauseModel {
       val simpleTags : List[List[(Int,Tag)]] = List(
      List(
     (0,  new Tag("B0XXXZ")),  
     (1,  new Tag("S0XXXX")),
     (0,  new Tag("B0XXXZ")),  
     (0,  new Tag("S2XXXX")),
     (2,  new Tag("B0XXXZ")),  
     (0,  new Tag("B0XXXJ")),
     (0,  new Tag("B1XXXZ")),  
     (1,  new Tag("S0XXXX"))
      ),
      List(
      (0, new Tag("B0XXXZ")),  
      (1, new Tag("S1XXXX")),
      (0, new Tag("B0XXXZ")),  
      (1, new Tag("S1XXXX")),
      (0, new Tag("B0XXXZ")),  
      (0, new Tag("S1CX,X")),
      (2, new Tag("B0XXXZ")),  
      (0, new Tag("S1XX,X"))
      ) 
     )
     
     override val getClauseTags = this.simpleTags
     override val toString : String =  super.toString
     
      override def getObserve(previous : Int, next : Int) : Int = next;
  }


@RunWith(classOf[JUnitRunner])
class ConditionClauseModelTest extends FunSuite {
     
   test("base class segmentCondition") {
     val simpleClass = new ConditionalClauseModelTest
     println(simpleClass.toString)
     
   }

      test("get existing prob") {
     val simpleClass = new SegmentConditionalModelTest
    
     println(simpleClass.getProbability(0, "S0XXXX","B0XXXZ"))
       println(simpleClass.getProbability(-1, "S0XXXX","B0XXXZ"))
     
   }
     test("non existing prob") {
     val simpleClass = new SegmentConditionalModelTest
     println(simpleClass.getProbability(2, "S0XXXX","B0XXXZ"))
     
   }
}
