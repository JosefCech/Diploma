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
import StatisticModul.StatisticEstimate

@RunWith(classOf[JUnitRunner])
class LevelModelTest2 extends FunSuite {
 
  test("init level segment") {
    val firstLevelModel = LevelModel
    
    println(firstLevelModel.toString)
    
  }
  
  test("init codition")
  {
    val firstModel = ConditionModel
     println(firstModel.toString)
  }
  
  
  test("init Estimam"){
    var test = new StatisticEstimate
    println(test.getConditionModel)
    println(test.getLevelModel)
  }
}