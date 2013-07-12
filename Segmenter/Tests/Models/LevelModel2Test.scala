package Models

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import segmenter._
import scala.xml._
import common._
import java.io.File
import StatisticModul.Models.LevelModel
import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LevelModelTest2 extends FunSuite {
 
  test("init level segment") {
    val firstLevelModel = new StatisticModul.Models.LevelModel
    
    println(firstLevelModel.toString)
    
  }
}