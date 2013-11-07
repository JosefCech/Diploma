package Models

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import segmenter._
import scala.xml._
import common._
import java.io.File
import StatisticModul.Models._
import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import StatisticModul.StatisticLevelEstimate
import StatisticModul.Models.ConditionalLevelModel

@RunWith(classOf[JUnitRunner])
class LevelModelTest2 extends FunSuite {
 
  test("init level segment") {
    val firstLevelModel = LevelModel
    
    println(firstLevelModel.toString)
    
  }
  
  test("init level codition")
  {
    val firstModel = ConditionalLevelModel
     println(firstModel.toString)
  }
  
  
  test("init level Estimam"){
    var test = new StatisticLevelEstimate
    println(test.getConditionModel)
    println(test.getLevelModel)
  }
  
   test("init clause segment") {
    val firstLevelModel = ClauseModel
    
    println(firstLevelModel.toString)
    
  }
  
  test("init clause codition")
  {
    val firstModel = ConditionalClauseModel
     println(firstModel.toString)
  }
  
  
  test("init clause Estimam"){
    var test = new StatisticLevelEstimate
    println(test.getConditionModel)
    println(test.getLevelModel)
  }
}