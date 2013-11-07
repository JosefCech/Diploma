package test

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.xml._
import statistic.LevelSegmentModel
import statistic.Models.ConditionalLevelModel


  class SegmentConditionalModelTest extends ConditionalLevelModel {
     val simpleTags : List[List[Tag]] = List(
      List(
       new Tag("B0XXXZ"),  
       new Tag("S0XXXX"),
       new Tag("B0XXXZ"),  
       new Tag("S0XXXX"),
       new Tag("B0XXXZ"),  
       new Tag("B0XXXJ"),
       new Tag("B0XXXZ"),  
       new Tag("S0XXXX")
      ),
      List(
       new Tag("B0XXXZ"),  
       new Tag("S0XXXX"),
       new Tag("B0XXXZ"),  
       new Tag("S0XXXX"),
       new Tag("B0XXXZ"),  
       new Tag("S1CX,X"),
       new Tag("B0XXXZ"),  
       new Tag("S1XX,X")
      ) 
     )
     override val getSimpleTags = this.simpleTags
     override val toString : String =  super.toString
      override def getObserve(previous : Int, next : Int) : Int = next;
  }


@RunWith(classOf[JUnitRunner])
class SegmentConditionTest extends FunSuite {
     
   test("base class segmentCondition") {
     val simpleClass = new SegmentConditionalModelTest
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