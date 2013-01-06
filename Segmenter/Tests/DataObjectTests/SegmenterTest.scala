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
    
    print(Segmenter.parsedSegments)
    println(Segmenter.parsedSegments.size)
    1
  }
}