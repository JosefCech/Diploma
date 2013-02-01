package Main

import java.io.File

object Clauser2 extends App {  
  def GetFiles(resFolder : String) : List[File] = {
     common.Directory.ReadAnxFiles(resFolder);
  }
  
 override def main(args: Array[String]) {
   
   val files = GetFiles("Results")
   val sentences = files.map(t => (t.getName,Anx.AnxReader.ReadSentence(t))).toList//.filter(p => p.size > 4).toList;
   val data = sentences.map(f => (f._1,(f._2,new RuleClauser(f._2).estimationOfClause)))
   println(sentences.map(f => new RuleClauser(f._2).analyzeSentence).flatten.filterNot(p => p._2.isBoundarySegment).map(p => (p._1,p._2.isBoundarySegment,p._2.segment.ToString)).size)
   val subclause = sentences.map(f => (f._1,new RuleClauser(f._2).analyzeSentence)).map(p => (p._1, p._2.filter(f => f._1 == 1 ))).filter(p => p._2.map(w => w._2.rest.toList.contains("ne�")).filter(t => t).toList.size > 0)
   		subclause.foreach(t => {println(t._1)
   								t._2.foreach(f => println(f._2.segment.ToString))
   		                        })
   		println(subclause.size)
   		/*
   data.foreach(f => { 
     f._2._1.foreach(t => println(t.ToString))
     println("odhad clause : " + f._2._2.toString + " pocet segmentu :" + f._2._1.size.toString)
   }) */
    
 }
  
  
  
}