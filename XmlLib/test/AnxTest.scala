
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
    data.Words.foreach(r => r match {case r : AnxWord => println(r.form,r.sep)})
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
      <word form="skute&#269;nost"/>
    </segment>
    <segment level="0" clausebeg="0">
      <word form="v&#353;ak" sep="1"/>
      <word form="nic"/>
      <word form="nem&#283;n&#237;"/>
      <word form="na"/><word form="faktu"/>
    </segment>
    <segment level="1" clausebeg="1">
      <word form="," sep="1"/>
      <word form="&#382;e" sep="1"/>
      <word form="nadch&#225;zej&#237;c&#237;"/>
      <word form="t&#253;dny"/>
    </segment><segment level="1" clausebeg="0">
      <word form="a" sep="1"/>
      <word form="m&#283;s&#237;ce"/>
      <word form="budou"/><word form="znamenat"/>
      <word form="ne&#250;m&#283;rn&#283;"/>
      <word form="zv&#253;&#353;en&#233;"/>
      <word form="n&#225;roky"/>
      <word form="na"/>
      <word form="administrativu"/>
      <word form="podnikatel&#367;"/>
      <word form="p&#345;i"/>
      <word form="rozv&#237;jen&#237;"/>
      <word form="jejich"/>
      <word form="obchodn&#237;ch"/>
      <word form="aktivit"/>
      <word form="se"/>
      <word form="slovensk&#253;mi"/>
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
