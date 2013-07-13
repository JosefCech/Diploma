package test

import statistic.Models.UnigClauseModel
import common.Tag
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.xml._

 class LevelTest2 extends UnigClauseModel {
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
  }

@RunWith(classOf[JUnitRunner])
class ClauseModelTest extends FunSuite {
  test("base class 21 ") {
     val simpleClass = new LevelTest2
     println(simpleClass.toString)
     
   }
   
   test("existing prob 2") {
        val simpleClass = new LevelTest2
        println(simpleClass.getProbability(0,"BXXXJ"))
   }
      test("unknown prob 2") {
        val simpleClass = new LevelTest2
        println(simpleClass.getProbability(1,"BCCCJ"))
   }
}