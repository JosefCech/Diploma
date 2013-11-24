package LevelEstimate

import Anx.AnxReader
import common.sentence.{ LevelEstimateSentence, AnalyzedSentence }
import common.segment.{ Segment, AnalyzedSegment}
import java.io.File
import DataStatistic.PackageInfo
import DataObjects.EstimateSentence
import Translation._
import java.io.PrintWriter
import Xml.XmlSentence
import com.sun.xml.internal.bind.marshaller.XMLWriter
import Xml.Writer
import common.Directory
import DataObjects.Estimation
import DataObjects.ResultsLevel

object LevelEstimate extends App {  
 override def main(args: Array[String]) {
    val dataSets = List("Develop","Heldout","Test")
    val printData = true
    dataSets.foreach(dataSet => this.analyzedSetData(dataSet, dataSet + "-file", "\t", printData))
   
 }
 
 def analyzedSetData(dataSet : String, headLine : String, prefix : String , print : Boolean) : Unit =
 {
    def files = common.Directory.ReadAnxFiles(segmenter.Configuration.DataFolder(dataSet)).toList
    // log data chybne ur�en� v�ty
    val pw = new java.io.PrintWriter(new File("logErrorLevel" + dataSet))
    val results = files.map(f => {
        // načtení věty
	    val sentence = AnxReader.ReadAnalyzedSentence(f)
	    // analýleza věty 
	    val analyzed = new LevelAnalyzedSentence(sentence.morfSentence)
	    // zápis analyzované věty
	    Estimation.writeSentence(analyzed,dataSet, "ruleLevelEstimation")
	    // porovnání analzyzované s výslednou
	    //(stejný počet segmenů, počet správně určených segmentů , počet  špatně určených segmentů , rozštěpení klauze, počet vět)
	    val result = compareSentence(sentence.analyzedSentence, analyzed)
	    if (result._3 > 0)
	     {
	        // zápis špatně anaotované věty
	        pw.write("---Start-----------\n")
	        pw.write(sentence.toString)
	        pw.write(analyzed.toString)
	        pw.write(analyzed.getLog)
	        pw.write("---End-------------\n")
	     }
	     result
    }).toList
    
    val resultsData = new ResultsLevel(results)
    resultsData.print(print, headLine, dataSet)
    Writer.Write("Result/"+"RuleLevel-" +dataSet + ".xml", resultsData)
    
 }

 
 def compareSentence(sentence : AnalyzedSentence , test : EstimateSentence ) :  (Boolean,Int,Int ,Boolean,Int) = 
  {
    def segmentsCompare(segments : List[AnalyzedSegment], testSegments : List[Segment], acc : (Int,Int)) 
    : (Int, Int) = 
    {
      if (segments.isEmpty || testSegments.isEmpty)
      {
        var wrong = 0;
        if (segments.isEmpty) wrong += testSegments.length
        if (testSegments.isEmpty) wrong += segments.length
        (acc._1,acc._2 + wrong)
      }
      else 
      {
       val a = segments.head 
       val t = testSegments.head
       if (a.LevelDefault == t.level.getExactLevel) segmentsCompare(segments.tail,testSegments.tail,(acc._1 + 1, acc._2 ))
       else segmentsCompare(segments.tail,testSegments.tail,(acc._1 , acc._2 + 1 ))      }
    } 
   val compare = segmentsCompare(sentence.segments,test.getEstimateSegments,(0,0))
   (sentence.segments.length == test.getEstimateSegments.length,compare._1,compare._2, PackageInfo.containsGap(sentence),PackageInfo.isComplexSentence(sentence))
  }
    
 }

