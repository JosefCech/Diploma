package DataObjectTests

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Segmenter._
import scala.xml._
import common._

@RunWith(classOf[JUnitRunner])
class SegmenterTest extends FunSuite {
  
  ("read morf sentence") {
    print(Segmenter.ParsedSegments("Data/cmpr9406_001.m"))
    println(Segmenter.ParsedSegments("Data/cmpr9406_001.m").size)
    1
  }
}