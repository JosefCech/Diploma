package test
import org.scalatest._
import org.scalatest.FunSuite	
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.xml._
import java.io._
import Anx.{ AnxWriter, AnxReader, AnxWord }
import common.{ Word, MorfWord }
import common.segment.{ Segment, PureSegment, Boundary }
import common.sentence.AnxSentence


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
    //data.Words.foreach(r => r match {case r : AnxWord => println(r.form,r.sep)})
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
   def anxWord = new AnxWord("form",true)
   def morfWord = new MorfWord("form","tag")
   assert(anxWord.equals(morfWord))
   }
   
   test("anx segment read") {
   def anxWord : Node = <segment><word tag="Z:-------------" form=","></word></segment>
   def segment = AnxReader.GetSegment(anxWord)
   segment.words.foreach(f => f match { case f : MorfWord => println(f.isSeparator)
   											  case f : Word => println(f.form + " " + false)
   })
   
   print(segment.words.map(f => f match { case f : MorfWord => f.isSeparator
   										  case f : Word =>  false
   										}).size)
   										
   segment match {
      case segment : PureSegment => println("PureSegment")
   }								
  }

    test("anx sentence read") {
      def anxSentenceXml : Node = <root>
  <sentence>
    <segment level="0" clausebeg="1">
      <word form="Tato"/>
      <word form="skutečnost"/>
    </segment>
    <segment level="0" clausebeg="0">
      <word form="však" sep="1"/>
      <word form="nic"/>
      <word form="nemění"/>
      <word form="na"/><word form="faktu"/>
    </segment>
    <segment level="1" clausebeg="1">
      <word form="," sep="1"/>
      <word form="že" sep="1"/>
      <word form="nadcházející"/>
      <word form="týdny"/>
    </segment><segment level="1" clausebeg="0">
      <word form="a" sep="1"/>
      <word form="měsíce"/>
      <word form="budou"/><word form="znamenat"/>
      <word form="neůměrně"/>
      <word form="zvýšené"/>
      <word form="nároky"/>
      <word form="na"/>
      <word form="administrativu"/>
      <word form="podnikatelů"/>
      <word form="při"/>
      <word form="rozvíjení"/>
      <word form="jejich"/>
      <word form="obchodních"/>
      <word form="aktivit"/>
      <word form="se"/>
      <word form="slovenskými"/>
      <word form="partnery"/>
    </segment>
    <segment level="0" clausebeg="1">
      <word form="." sep="1"/>
    </segment>
  </sentence>
</root>
        
   val segments = anxSentenceXml \\"segment" 
   val anxSentence = new AnxSentence(segments.map(t => AnxReader.GetSegment(t,true)).toList)
   println(anxSentence.toString)   
   }
 
}
