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
            println(directory.getAbsoluteFile)
            List[File]()   
    }
  }

  def ReadAnxFiles( dir : String) : List[File] = {
   ReadAllFiles(dir,".anx")
  }
  
  def ReadXmlFiles(dir : String) : List[File] = {
    ReadAllFiles(dir,".xml")
  }
  
  def ReadSegFiles(dir : String ) : List[File] = {
    ReadAllFiles(dir,".seg")
  }
  
  def ReadPdtMorfFiles(dir:String) : List[File] = {
     ReadAllFiles(dir,".m")
  }
  
  def ReadAnalyticFiles(dir : String) : List[File] = {
     ReadAllFiles(dir,".a")
  }
  def ReadFiles(dir : String) : List[File] = {
     ReadAllFiles(dir, "")
    
    
  }
}