package test

import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import wordProperties._

@RunWith(classOf[JUnitRunner])
class TagMatcherTest extends FunSuite {

  test("base match ") {
  def word=  new MorfWord("form","Vf_____________")
  def word1=  new MorfWord("form","VfI____________")
  assert(TagMatcher.ExactMatch(word.tag, word.tag))
  assert(TagMatcher.Match(word.tag, word1.tag))
  assert(TagMatcher.Match(word, word.tag))
  }
  
  test("match words - set Vi , _A") {
  def words = List( 
      new MorfWord("formVF","Vf_____________"),
      new MorfWord("formvi","Vi_____________"),
      new MorfWord("formN","NN_____________"),
      new MorfWord("formA","AA_____________")
      ) 
  def set = List("Vi","_A")
    TagMatcher.MatchSetExcept(words, set, List[String]()).foreach(f => println(f.form))
  }
 
  
   
      test("match words - set V , _A except VI") {
  def words = List( 
      new MorfWord("formVF","Vf_____________"),
      new MorfWord("formvi","Vi_____________"),
      new MorfWord("formN","NN_____________"),
      new MorfWord("formA","AA_____________")
      ) 
  def set = List("V","_A")
  def except = List("Vi")
  
  TagMatcher.MatchSetExcept(words, set, except).foreach(f => println(f.form))
    assert(TagMatcher.MatchSetExcept(words, set, except).size == 2)
  }
}