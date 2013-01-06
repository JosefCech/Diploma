package Segmenter
import java.io._

object Separators extends App {

  def files : Array[File] =  { 
    val f = new File("data")   
    if (f.isDirectory)  f.listFiles
    else Array[File]{f}   
  }
  
 files. 
}