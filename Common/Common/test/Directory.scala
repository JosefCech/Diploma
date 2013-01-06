package test

import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.xml._
import java.io._



@RunWith(classOf[JUnitRunner])
class DictionaryTest extends FunSuite {
  
 test("non exists folder test") {
  def files : List[File] = Directory.ReadFiles("Non/Exist/Folder")
  assert(files.size == 0 )
 }
   
 test("read all content of folder") {
   def files : List[File] = Directory.ReadFiles("Data")
   files.size == 3  
 }
 
  test("read anx content of folder") {
   def files : List[File] = Directory.ReadAnxFiles("Data")
   files.size == 1  
 }
  
 test("read xml content of folder") {
   def files : List[File] = Directory.ReadXmlFiles("Data")
   files.size == 1  
 } 
}
