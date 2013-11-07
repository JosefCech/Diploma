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
  
  test("morfword test tag compare") {
    def word1 = new MorfWord("form","tag")
    assert(word1.compareTag("tag"))
    assert(word1.compareTag("ta"))
    assert(word1.compareTag("t"))
  }
  
   test("get subflag conjuction") {
    def words = List( 
      new MorfWord("formVF","Vf_____________"),
      new MorfWord("formvi","Vi_____________"),
      new MorfWord("formN","NN_____________"),
      new MorfWord("formA","AA_____________"),
      new MorfWord("formp6","P6_____________"),
      new MorfWord("formN","J^_____________"),
      new MorfWord("formA","AA_____________"),
       new MorfWord("n�","P9FS6----------")
      ) 
    words.filter(t => t.isSubFlag).foreach(f => println(f.form))
     
   assert(true)
  }
   
   test("rule word") {
     val word = new RuleWord("", "P")
     val word2 = new RuleWord("ja", "P")
     val morfWord = new MorfWord("n�","P9FS6----------")
     println(word.equals(morfWord))
     println(wordProperties.TagMatcher.Match(morfWord, "P--------------"))
     assert(word.equals(morfWord))
     assert(!word2.equals(morfWord))
   }
}
