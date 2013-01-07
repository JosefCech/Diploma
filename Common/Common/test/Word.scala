package  test 

import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.xml._



@RunWith(classOf[JUnitRunner])
class WordTest extends FunSuite {
  
 test("word test") {
  def words =  new Word("form")
  
  assert(words.form == "form")
 }
   
 test("morfword test") {
  def words =  new MorfWord("form","tag")
  
  assert(words.form == "form")
  assert(words.tag == "tag")
 }
 
  test("morfword test  equals ") {
  def words =  new MorfWord("form","tag")
  def words2 =  new MorfWord("form","tag")
  assert(words.equals(words2))
 }
  
  test("morfword test  equals - word ") {
  def words =  new MorfWord("form","tag")
  def words2 =  new Word("form")
  assert(words.equals(words2))
 }
  
  test("morfword test  equals - string") {
  def words =  new MorfWord("form","tag")
  def words2 =  "form"
  assert(words.equals(words2))
 }
}
