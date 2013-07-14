package test

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.xml._
import statistic.Viterbi
import statistic.Models.UnigLevelModel
import statistic.Models.ConditionalLevelModel
import statistic.BaseModel.UnigModel
import statistic.BaseModel.ConditionalModel
import statistic.Models.UnigClauseModel
import statistic.Models.ConditionalClauseModel
import statistic.LevelViterbi
import statistic.ClauseViterbi

class ViterbiInstanceLevel(val levelModel : UnigLevelModel , val conditionModel : ConditionalLevelModel) extends LevelViterbi
{
 override val getLevelModel : UnigModel = this.levelModel
 override val getConditionModel : ConditionalModel = this.conditionModel
 
 override val getAllStates: List[Int] = (0 to 5).toList
 
}

class ViterbiInstanceClause(val levelModel : UnigClauseModel , val conditionModel : ConditionalClauseModel) extends ClauseViterbi
{
 override val getLevelModel : UnigModel = this.levelModel
 override val getConditionModel : ConditionalModel = this.conditionModel
 
 override val getAllStates: List[Int] = (0 to 5).toList
 
}

@RunWith(classOf[JUnitRunner])
class ViterbiTest extends FunSuite{
 test("viterbi instance level") {
   
   val level  = new LevelTest
   val condition = new SegmentConditionalModelTest
   val viterbi = new ViterbiInstanceLevel(level ,condition)
    
   println(viterbi.getBestPath(List(new Tag("S1CX,X"), new Tag("B0XXXZ"),new Tag("S1CX,X")), List((1.0,List((-1,(new Tag("*"),new Tag("*"))))))))
 }
 
  test("viterbi instance clause") {
   
   val level  = new ClauseTest2
   val condition = new ConditionalClauseModelTest
   val viterbi = new ViterbiInstanceClause(level ,condition)
    
   println(viterbi.getBestPath(List(new Tag("S1CX,X"), new Tag("B0XXXZ"),new Tag("S1CX,X")), List((1.0,List((-1,(new Tag("*"),new Tag("*"))))))))
 }
 
}