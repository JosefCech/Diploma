package Main

import java.io.File

object Clauser extends App {  
  def GetFiles(resFolder : String) : List[File] = {
     common.Directory.ReadAnxFiles(resFolder);
  }
  
 override def main(args: Array[String]) {
   
   val files = GetFiles("Results")
   val sentences = files.map(t => (t.getName,Anx.AnxReader.ReadSentence(t))).toList//.filter(p => p.size > 4).toList;
   val data = sentences.map(f => (f._1,(f._2,new RuleClauser(f._2).estimationOfClause)))
   println(sentences.map(f => new RuleClauser(f._2).analyzeSentence).flatten.filterNot(p => p._2.isBoundarySegment).toList)
   val subclause = sentences.map(f => new RuleClauser(f._2).analyzeSentence).map(p => p.filter(f => f._1 == 1)).flatten
   		subclause.foreach(t => println(t._2.segment.ToString))
   		println(subclause.size)
   		/*
   data.foreach(f => { 
     f._2._1.foreach(t => println(t.ToString))
     println("odhad clause : " + f._2._2.toString + " pocet segmentu :" + f._2._1.size.toString)
   }) */
    
 }
  
  
  
}

