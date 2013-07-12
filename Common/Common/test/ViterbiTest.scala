package test

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.xml._
import statistic.LevelSegmentModel
import statistic.SegmentConditionalModel
import statistic.Viterbi

class ViterbiInstance(val levelModel : LevelSegmentModel , val conditionModel : SegmentConditionalModel) extends Viterbi
{
 override val getLevelModel : LevelSegmentModel = this.levelModel
 override val getConditionModel : SegmentConditionalModel = this.conditionModel
 
 override val getAllStates: List[Int] = (0 to 5).toList
 
}

@RunWith(classOf[JUnitRunner])
class ViterbiTest extends FunSuite{
 test("viterbi instance") {
   
   val level  = new LevelTest
   val condition = new SegmentConditionalModelTest
   val viterbi = new ViterbiInstance(level ,condition)
    
   println(viterbi.getBestPath(List(new Tag("S1CX,X"), new Tag("B0XXXZ"),new Tag("S1CX,X")), List((1.0,List((-1,(new Tag("*"),new Tag("*"))))))))
 }
}