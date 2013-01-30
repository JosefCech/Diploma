package test

import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import wordProperties._

@RunWith(classOf[JUnitRunner])
class TagQueryTest extends FunSuite{

  test("get active verbs") {
    def words = List( 
      new MorfWord("formVF","Vf_____________"),
      new MorfWord("formvi","Vi_____________"),
      new MorfWord("formN","NN_____________"),
      new MorfWord("formA","AA_____________")
      ) 
     TagQuery.activeVerb(words).foreach(t => println(t.form))
   assert(TagQuery.activeVerb(words).size == 1)
  }
  
    test("get cord conjuction") {
    def words = List( 
      new MorfWord("formVF","Vf_____________"),
      new MorfWord("formvi","Vi_____________"),
      new MorfWord("formN","NN_____________"),
      new MorfWord("formA","AA_____________"),
      new MorfWord("formp6","P6_____________"),
      new MorfWord("formN","J^_____________"),
      new MorfWord("formA","AA_____________")
      ) 
     TagQuery.cordConjuction(words).foreach(t => println(t.form))
   assert(TagQuery.cordConjuction(words).size == 1)
  }
    
     test("get reflexive pronoun") {
    def words = List( 
      new MorfWord("formVF","Vf_____________"),
      new MorfWord("formvi","Vi_____________"),
      new MorfWord("formN","NN_____________"),
      new MorfWord("formA","AA_____________"),
      new MorfWord("formp6","P6_____________"),
      new MorfWord("formN","J^_____________"),
      new MorfWord("formA","AA_____________")
      ) 
     TagQuery.reflexivePronoun(words).foreach(t => println(t.form))
   assert(TagQuery.reflexivePronoun(words).size == 1)
  }
  }
