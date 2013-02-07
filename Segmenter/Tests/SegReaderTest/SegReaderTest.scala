package SegReaderTest

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Segmenter._
import scala.xml._
import common._

@RunWith(classOf[JUnitRunner])
class SegReaderTest extends FunSuite {
  
  test("read  sentence") {
   SegReader.ReadData("SegData/cmpr9406-001-p4s3.seg").foreach(f => println(f._1 + " " +f._2))
    1
  }
}