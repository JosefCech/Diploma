package Test

import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import Main._

@RunWith(classOf[JUnitRunner])
class WordFormQueryTest extends FunSuite{

    def words = List( 
      new MorfWord("{","Z:_____________"),
      new MorfWord("(","Z:_____________"),
      new MorfWord("[","Z:_____________"),
      new MorfWord("}","Z:_____________"),
      new MorfWord(")","Z:_____________"),
      new MorfWord("]","Z:_____________"),
      new MorfWord("'","Z:_____________"),
      new MorfWord("\"","Z:_____________"),
      new MorfWord("!","Z:_____________"),
      new MorfWord("?","Z:_____________")
      ) 
      
  test("get close bracket") {
    
     WordFormQuery.closeBracket(words).foreach(t => println(t.form))
   assert(WordFormQuery.closeBracket(words).size == 3)
  }
  
    test("get open bracket") {
   
  WordFormQuery.openBracket(this.words).foreach(t => println(t.form))
   assert( WordFormQuery.openBracket(this.words).size == 3)
  }
    
     test("get reflexive pronoun") {
   
    WordFormQuery.quotationMark(this.words).foreach(t => println(t.form))
   assert( WordFormQuery.quotationMark(this.words).size == 2)
  }
  }
