package Stats
import common._
import java.io._
import xml._
import Pdt.MorfReader

class BaseInfo(string : String) {
  val segDir= string + "/seg"
  val morfDir = string + "/morf"
  
  val segFiles = Directory.ReadSegFiles(segDir)
  val segFilesNames = segFiles.map(f =>  f.getName.replace(".seg", ""))
  val morfFiles = segFiles.map(f => morfDir + "/" + f.getName().split("-").head +"_"+ f.getName().split("-").tail.head + ".m")
  				  .toList.distinct.map(t => new File(t)).toList.filter(p => p.exists)
  
  val segData = segFiles.map(f => SegReader.ReadData(f))				  
  val goldenSet =	morfFiles.map(t => MorfReader.Read(t) ).flatten
  					.map(p => {
  					  val ident = p.ident.replace("m-", "")
  					  val data = segData.filter(t => t._1.trim == ident.trim)
  						if (!data.isEmpty) {					  
  						  ( data.head , p)
  						}
  						else {
  						 ((ident,null),p)
  						}
  					}).toList.filterNot(p => p._1._2 == null).toList 	
  					
  val maxLevel = segData.map(f => f._2.map(t => t._1).toList.reduceRight((x,v) => if (x > v) x else v)).toList.reduceRight((x,v) => if (x > v) x else v)
  val countSegments = goldenSet.map(f => f._2.parsedSegments(f._1._2.toList)).flatten.size
  val noVerbSegments = goldenSet.map(s => s._2.parsedSegments(s._1._2.toList).map(m => new AnalyzedSegment(m)).filter(m => m.haveActiveVerb)).flatten
  val errors = goldenSet.filter(p => p._1._2.size != p._2.parsedSegments(List[(Int,Int)]()).size).toList
  val clauseEstimatedCount = 
     	goldenSet.map(t => (t._1._1,t._2.parsedSegments(t._1._2.toList).map(s => new AnalyzedSegment(s)))).map(s => {
     		val countActiveVerb = s._2.filter(p => p.haveActiveVerb).size
     		val countSegments = s._2.size
     		if (countActiveVerb == 0) {
     		  (s._1,countSegments,1)
     		}
     		else {
     			(s._1,countSegments,s._2.filter(p => p.haveActiveVerb).size)
     		}
   		}).groupBy(f => f._3).map(f => (f._1,
   		    f._2.groupBy(f => f._2).map(f => (f._1,f._2.size)).toList.sortBy(f => f._1)
   		    )).toList.sortBy(f => f._1)
  val subFlagsStats = goldenSet.map(t => (t._1._1,t._2.parsedSegments(t._1._2.toList).map(s => new AnalyzedSegment(s)))).map(s => { 
      val countSubflags = s._2.filter(p => p.haveSubFlag).size
      val countSegments = s._2.filter(p => p.segment.level != 0).size
      (countSegments,countSubflags)
    }  
  ).toList.groupBy(f => f).map(f => (f._1,f._2.size)).toList
  
  val sublflagsDetail = goldenSet.map(t => (t._1._1,t._2.parsedSegments(t._1._2.toList).map(s => new AnalyzedSegment(s)))).map(s => s._2).
  						flatten.toList.filter(p => (!p.isBoundarySegment && p.segment.level == 0 && (p.haveSubFlag))).map(s => (s.segment.words.head.form, s.segment.ToString)).toList.
  						groupBy(s => s._1).toList
}