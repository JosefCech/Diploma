
import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.xml._
import java.io._
import Anx.AnxWriter
import Anx.AnxReader
import Anx.AnxWord
import common.Word
import Anx.AnxReader


@RunWith(classOf[JUnitRunner])
class AxnTest extends FunSuite {

  def GetWords(count : Int) :  Segment = 
  {
    def GetWordsAcc(count : Int, acc : List[Word]) : Segment = {
      if (count == 0) new PureSegment(acc)
      else {
        def w1 = new MorfWord("testform" + count ,"tag" + count)
        GetWordsAcc(count-1, w1 :: acc )  
        }
      
    }
    
    GetWordsAcc(count,List[Word]())
  }
  
  def GetSegments(count : Int , countWord : Int) : List[Segment] = {
    def GetSegmentsAcc(count : Int , countWord : Int , acc : List[Segment]) : List[Segment] = {
      if (count == 0) acc
      else {
         def words = GetWords(countWord)
         GetSegmentsAcc(count-1,countWord,words :: acc)
      }  
    }
    GetSegmentsAcc(count,countWord,List[Segment]())
  }
  
  test("read anx file ca-001.xml")
  {
    val file = new File("ca-001.anx")
    val data = AnxReader.ReadSentence(file)
    data.foreach(t => t.words.foreach(r => r match {case r : AnxWord => println(r.form,r.tag,r.sep)}))
  }
  
 test("create segment node") {
  def segments =  GetSegments(2,5);
  def xmlSegments =  new Xml.XmlSentence(segments)
  println(xmlSegments.TransformXml)
 }
 
  test("write anx file") {
  def segments =  GetSegments(2,2)
  def f = "testdata.anx"
  AnxWriter.Write(f,segments)
 }
  
   test("anx word equals morfword") {
   def anxWord = new AnxWord("form","tag",true)
   def morfWord = new MorfWord("form","tag")
   assert(anxWord.equals(morfWord))
   }
   
   test("anx word read") {
   def anxWord : Node = <segment><word tag="Z:-------------" form=","></word></segment>
   def segment = AnxReader.GetSegment(anxWord)
   segment.words.foreach(f => f match { case f : MorfWord => println(f.isSeparator)
   											  case f : Word => println(f.form + " " + false)
   })
   
   print(segment.words.map(f => f match { case f : MorfWord => f.isSeparator
   										  case f : Word =>  false
   										}).size)
   										
   segment match {
     case segment : Boundary => println("Boundary")
     case segment : PureSegment => println("PureSegment")
   }								
  }

 
}
