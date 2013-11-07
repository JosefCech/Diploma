package  test 

import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.xml._



@RunWith(classOf[JUnitRunner])
class LemmaTest extends FunSuite {
  
 test("create lemma test") {
  def lemma =  new Lemma("form")
  def lemma1 = new Lemma("form_(^2)")
  
 Console.print(lemma.lemma)

  assert(lemma.LemmaForm == "form")
  assert(lemma1.LemmaForm == "form")
  assert(lemma.LemmaFullForm == "form")
  assert(lemma1.LemmaFullForm == "form_(^2)")
 }
   
 test("compare lemma test") {
   def lemma =  new Lemma("form")
   def lemma1 = new Lemma("form_(^2)")
   
   assert(lemma == "form")
   assert(lemma1 == "form")
 }
 
 test(" morfword with lemma") {
    def word = new MorfWord("form","form_(^1)","NN--------------")
    def word2 = new MorfWord("form",new Lemma("form_(^1)"),"NN--------------")
    def word3 = new MorfWord("form",new Lemma("form_(^2)"),"NN--------------")
    def word4 = new MorfWord("form","NN--------------")
    
     assert(word == word2)
     assert(word != word3)
     assert(word == word4)
 }
 
}
