package segmenter

import scala.xml._
import scala._
object Configuration {
  
  val root =   XML.loadFile("configuration.xml") 
  def DataFolder : String =  {
   Configuration.LoadAttributeFolder((root \ "Separators").head.head)    
  }
   def SegDataFolder : String =  {
       Configuration.LoadAttributeFolder((root \ "SegmentationData").head.head)   
  }
   def  PdtDataFolder : String =  {
     Configuration.LoadAttributeFolder((root \ "PdtData").head.head)   
    }
  def OutputGoldenFolder : String =  {
    Configuration.LoadAttributeFolder((root \ "OutputGolden").head.head)   
  }
  def OutputOthersFolder : String =  {
    Configuration.LoadAttributeFolder((root \ "OutputOthers").head.head)   
  } 
  def ClauseDataGold : String = {
    Configuration.LoadAttributeFolder((root \ "ClauseGoldData").head.head)   
  }
  def AnxDataFolder : String = {
    Configuration.LoadAttributeFolder((root \ "AnxDataFolder").head.head)     
  }
  
  def RuleFile : String = {
     Configuration.LoadAttributeFile((root \ "RuleFile").head.head)
  }
  
  def DataFolder(dev : String) = {
     val folder = dev + "Data"
     Configuration.LoadAttributeFolder((root \ folder).head.head)
    
  } 
  
 def LoadAttributeFolder(elem : Node) : String = Configuration.LoadAttribute(elem,"@Folder")   
 def LoadAttributeFile(elem : Node) : String = Configuration.LoadAttribute(elem,"@File") 
  
 def LoadAttribute(elem : Node, attr : String ) : String = {
   (elem \ attr).toString() 
 }
}