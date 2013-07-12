package test

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.xml._
import statistic.LevelSegmentModel



@RunWith(classOf[JUnitRunner])
class LevelModelTest extends FunSuite {
 
  class LevelTest extends LevelSegmentModel {
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
  }
   
   test("base class") {
     val simpleClass = new LevelTest
     println(simpleClass.toString)
     
   }
   
   test("existing prob") {
        val simpleClass = new LevelTest
        println(simpleClass.getProbability(0,"BXXXJ"))
   }
      test("unknown prob") {
        val simpleClass = new LevelTest
        println(simpleClass.getProbability(1,"BCCCJ"))
   }
}