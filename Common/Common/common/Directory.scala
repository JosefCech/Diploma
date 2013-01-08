package common

import java.io._

object Directory {
   
   def ReadAllFiles( dir : String ,  typeFile : String) : List[File] = {

    def directory = new File(dir)
    if (directory.exists()) {
      
	if (typeFile!=""){
	     directory.listFiles filterNot (_.isDirectory) filter (t => t.getName().endsWith(typeFile)) toList
    }
   else {
	   	directory.listFiles filterNot (_.isDirectory) toList
   }
   }
    else {
            println("Missing folder exception")
            List[File]()   
    }
  }

  def ReadAnxFiles( dir : String) : List[File] = {
   ReadAllFiles(dir,".anx")
  }
  
  def ReadXmlFiles(dir : String) : List[File] = {
    ReadAllFiles(dir,".xml")
  }
  
  def ReadFiles(dir : String) : List[File] = {
     ReadAllFiles(dir, "")
    
    
  }
}