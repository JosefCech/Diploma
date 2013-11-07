package test

import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common.Tag
import wordProperties._

@RunWith(classOf[JUnitRunner])
class TagTest extends FunSuite{
  
  val tag1 : Tag=  new Tag("B0--J")
  val tag2 : Tag = new Tag("C1-JK")
  val tag3 : Tag = new Tag("S2C-X")
  
  test("type of segment") {

    assert(tag1.isBoundary)
    assert(tag2.isBoundary)
    assert(!tag3.isBoundary)
  }
  
  test("level of segment")
  {

    assert(tag1.Level == 0)
    assert(tag2.Level == 1)
    assert(tag3.Level == 2)
  }
  
  test("have verb")
  {
    assert(!tag1.haveActiveVerb)
    assert(!tag2.haveActiveVerb)
    assert(tag3.haveActiveVerb)
  }
  
  test("have subflag")
  {
    assert(tag1.haveSubflag)
    assert(tag2.haveSubflag)
    assert(!tag3.haveSubflag)
  }
}