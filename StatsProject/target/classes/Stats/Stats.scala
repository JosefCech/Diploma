package Stats
import common._
import common.sentence._
import common.segment.AnalyzedSegment
import java.io._
import xml._

import Pdt.MorfReader
object Stats extends App {
  val segFiles = Directory.ReadSegFiles("../firsttry/SVN/data/pdt/train2/golden")
  val segFilesNames = segFiles.map(f =>  f.getName.replace(".seg", ""))
  val morfFiles = segFiles.map(f => "../firsttry/SVN/data/pdt/train2/" + f.getName().split("-").head +"_"+ f.getName().split("-").tail.head + ".m")
  				  .toList.distinct.map(t => new File(t)).toList.filter(p => p.exists)
  
  val segData = segFiles.map(f => SegReader.ReadData(f))				  
  val goldenSet =	morfFiles.map(t => MorfReader.Read(t) ).flatten
  					.map(p => {
  					  val ident = p.ident.replace("m-", "")
  					//  println(ident + " " + segData.filter(t => t._1.trim == ident.trim) +" " + segData.filter(p => (p._1.split("-").head == ident.split("-").head) &&  (p._1.split("-").apply(2) == ident.split("-").apply(2))))
  					  val data = segData.filter(t => t._1.trim == ident.trim)
  						if (!data.isEmpty) {					  
  						  ( data.head , p)
  						}
  						else {
  						 ((ident,null),p)
  						}
  					}).toList.filterNot(p => p._1._2 == null).toList 				  
  baseInformation(segData)
  segInformation(goldenSet)					
  		

 

 def baseInformation(segData : List[(String, Array[(Int, Int)])])  =  {
    println("Nejhlubší level : ")
    val maxLevel = segData.map(f => f._2.map(t => t._1).toList.reduceRight((x,v) => if (x > v) x else v)).toList.reduceRight((x,v) => if (x > v) x else v)
    println(maxLevel);
    println("Počty segmentů : ")
    val groupSegments = segData.map(t => t._2.size).groupBy(x => x).map(x => (x._1,x._2.size)).toList.sortBy(_._1 ).toList
    groupSegments.foreach(f => println("Po�et segment�:" + f._1.toString + " po�et v�t: " + f._2))
   
     val groupSegmentsDetail = segData.map(t => t._2.map(t => t._1).toList).groupBy(f => f)
     						.map(f => (f._1.size,f._1,f._2.size)).groupBy(f => f._1).map(t => (t._1,t._2.foldLeft(0)((a,b) => a + b._3),t._2.map(r => (r._3,r._2))))   
     groupSegmentsDetail.foreach(m => {
    	 		println("Po�et segment�:" + m._1.toString + " po�et v�t: " + m._2)
    	 		m._3.foreach(n => println("\t Konfigurace :" + n._2 + " po�et : " + n._1 ))
    	 		
     } )
    
    val pairsLevel = segData.map(f => f._2.map(t => t._1).toList.foldLeft((-1,List[(Int,Int)]()))((a,b) => (b,(a._1,b) :: a._2))._2)
    				.flatten.filter(t => t._1 != -1).groupBy(f => f).map(f => (f._1,f._2.size)).toList.sortBy(f => f._1)
    val levelDiff = pairsLevel.groupBy(f => f._1._1-f._1._2).toList.sortBy(t=> t._1)
    
   levelDiff.foreach(f =>  {
     val wholeCount = f._2.foldLeft(0)((a,b) => a + b._2)
     println("Rozd�l " + f._1 + " celkov� po�et dvojic " + wholeCount + " dojice : " + f._2)
   })
   
   val jumpsLevel = pairsLevel.filter(p => p._1._2 < p._1._1).groupBy(f => f._1._2).toList.sortBy(p => p._1)
  }

 def segInformation(goldenSet : List[((String,Array[(Int,Int)]),MorfSentence)]) = {
   val segmentErrors = goldenSet.filter(p => p._1._2.size != p._2.parsedSegments(List[(Int,Int)]()).size).toList
   println(goldenSet.map(f => f._2.parsedSegments(f._1._2.toList)).flatten.size)
   segmentErrors.foreach(f => println("Sentence Ident "+f._1._1+"Tagged :" + f._1._2.size + " Segmented : " + f._2.parsedSegments(List[(Int,Int)]()).size + " Sentence : " + f._2.parsedSegments(List[(Int,Int)]()).map(t => t.toString)))
   val clauseEstimatedCount = 
     	goldenSet.map(t => (t._1._1,t._2.parsedSegments(t._1._2.toList).map(s => new AnalyzedSegment(s)))).map(s => {
     		val countActiveVerb = s._2.filter(p => p.getInfoSegment.HaveActiveVerb).size
     		if (countActiveVerb == 0) {
     		  (s._1,1)
     		}
     		else {
     			(s._1,s._2.filter(p => p.getInfoSegment.HaveActiveVerb).size)
     		}
   		}).groupBy(f => f._2).map(f => (f._1,f._2.size)).toList.sortBy(f => f._1)
   								
   println(clauseEstimatedCount)
   
   val noVerbSegments = goldenSet.map(s => s._2.parsedSegments(s._1._2.toList).map(m => new AnalyzedSegment(m)).filter(m => m.getInfoSegment.HaveActiveVerb)).flatten
   println(noVerbSegments.size)
  // println(noVerbSegments.groupBy(f => f).map(f => f._2.LevelDefault(f._1,f._2.size)))
   println(noVerbSegments.filter(p => p.LevelDefault == 0).size)
 }


}
