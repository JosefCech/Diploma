package DataObjectTests 

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.xml._
import Anx.AnxWriter
import DataObjects.BaseXmlSentence
import DataObjects.BaseXmlSentence

@RunWith(classOf[JUnitRunner])
class SentenceTest extends FunSuite {
 
 def GetWords(count : Int) :  List[Word] = 
  {
    def GetWordsAcc(count : Int, acc : List[Word]) : List[Word] = {
      if (count == 0) acc
      else {
        def w1 = new MorfWord("testform" + count ,"tag" + count)
        GetWordsAcc(count-1, w1 :: acc )  
        }
      
    }
    
    GetWordsAcc(count,List[Word]())
  }
  
  def GetSegments(count : Int , countWord : Int) : List[List[Word]] = {
    def GetSegmentsAcc(count : Int , countWord : Int , acc : List[List[Word]]) : List[List[Word]] = {
      if (count == 0) acc
      else {
         def words = GetWords(countWord)
         GetSegmentsAcc(count-1,countWord,words :: acc)
      }  
    }
    GetSegmentsAcc(count,countWord,List[List[Word]]())
  }
  
 test("write xml file - XmlWritable object") {
  def segments =  GetSegments(4,4);
  def sentence = new BaseXmlSentence(segments)
  assert(AnxWriter.Write("TestResource/text2.xml", sentence))
 }
}