import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import Pdt._
import scala.xml._
import java.io._

@RunWith(classOf[JUnitRunner])
class AReaderTest extends FunSuite {
  
  test("analytic reader") {
   
  def data =  AReader.CreateTrees( new File("cmpr9406_001.a") )
   //println(data.size)
   0
 }
  
  
   test("analytic reader - create inner node") {
   def data = <LM id="a-cmpr9406-002-p2s1Aw1">
               <m.rf>m#m-cmpr9406-002-p2s1Aw1</m.rf>
               <afun>ExD</afun>
               <ord>1</ord>
		   	   <clause_number>1</clause_number>
              </LM>
   println(AReader.CreateNode(data).toString)
   0
 }
  test("analytic reader - create root node") {
   def data =  <LM id="a-cmpr9406-002-p2s1A">
		   		<s.rf>m#m-cmpr9406-002-p2s1A</s.rf>
		   		<ord>0</ord>
               </LM>
   println(AReader.CreateTree(data).root)
   0
 }
  
  test("analytic reader - create tree") {
   def data =  <LM id="a-cmpr9406-001-p2s1">
      <s.rf>m#m-cmpr9406-001-p2s1</s.rf>
      <ord>0</ord>
      <children>
        <LM id="a-cmpr9406-001-p2s1w2">
          <m.rf>m#m-cmpr9406-001-p2s1w2</m.rf>
          <afun>ExD</afun>
          <ord>2</ord>
          <clause_number>1</clause_number>
          <children>
            <LM id="a-cmpr9406-001-p2s1w1">
              <m.rf>m#m-cmpr9406-001-p2s1w1</m.rf>
              <afun>Adv</afun>
              <ord>1</ord>
              <clause_number>1</clause_number>
            </LM>
            <LM id="a-cmpr9406-001-p2s1w3">
              <m.rf>m#m-cmpr9406-001-p2s1w3</m.rf>
              <afun>AuxC</afun>
              <ord>3</ord>
              <clause_number>1</clause_number>
              <children>
                <LM id="a-cmpr9406-001-p2s1w4">
                  <m.rf>m#m-cmpr9406-001-p2s1w4</m.rf>
                  <afun>ExD</afun>
                  <ord>4</ord>
                  <clause_number>1</clause_number>
                </LM>
              </children>
            </LM>
          </children>
        </LM>
      </children>
    </LM>
   println(AReader.CreateTree(data).root.GetChildren.head.GetChildren)
   0
 }
}