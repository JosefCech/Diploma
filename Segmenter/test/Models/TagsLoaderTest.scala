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
class TagsLoaderTest extends FunSuite with LoaderTags{

  test("load tags only") {
  (this.LoadTags)
  }
    test("load tags with clause") {
    (this.LoadTagsWithClause)
  }
}