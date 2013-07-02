package test

import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import wordProperties._

@RunWith(classOf[JUnitRunner])
class TagTest extends FunSuite{
  
  val tag1 : Tag= new Tag("B")
  val tag2 : Tag = new Tag("C")
  val tag3 : Tag = new Tag("S")
  
  test("type of segment") {

    assert(tag1.isBoundary)
    assert(tag2.isBoundary)
    assert(tag3.isBoundary)
  }

}