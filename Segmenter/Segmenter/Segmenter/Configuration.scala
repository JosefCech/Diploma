package segmenter

import scala.xml._
import scala._
object Configuration {
  
   val root =   XML.loadFile("configuration.xml") 
  def DataFolder : String =  {
      val separator = (root\"separatorDir")  
      separator.head.text.trim()
  }

}