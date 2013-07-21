package LevelEstimate

import Anx.AnxReader
import common.sentence.{ LevelEstimateSentence, AnalyzedSentence }
import common.segment.{ Segment, AnalyzedSegment}
import java.io.File
import DataStatistic.PackageInfo
import DataObjects.EstimateSentence

object LevelEstimate extends App {
  
 override def main(args: Array[String]) {
    val dataSets = List("Develop","Heldout","Test")
    val printData =true
    dataSets.foreach(dataSet => this.analyzedSetData(dataSet, dataSet + "-file", "\t", printData))
   
 }
 
 def analyzedSetData(dataSet : String, headLine : String, prefix : String , print : Boolean) : Unit =
 {
    def files = common.Directory.ReadAnxFiles(segmenter.Configuration.DataFolder(dataSet)).toList
    // log data chybne urèené vìty
    val pw = new java.io.PrintWriter(new File("logErrorLevel" + dataSet))
    val results = files.map(f => {
        // naètení vìty
	    val sentence = AnxReader.ReadAnalyzedSentence(f)
	    // analýza vìty bez naètených informací
	    val analyzed = new LevelAnalyzedSentence(sentence.morfSentence)
	    // porovnání naètené a analyzované vìty
	    //(stejný poèet segmentù,poèet správnì urèených segmentù , poèet špatnì urèených segmentù , rozštìpená klauze, poèet vìt)
	    val result = compareSentence(sentence.analyzedSentence, analyzed)
	    if (result._3 > 0)
	     {
	        // zápis špatnì anotované vìty
	        pw.write("---Start-----------\n")
	        pw.write(sentence.toString)
	        pw.write(analyzed.toString)
	        pw.write(analyzed.getLog)
	        pw.write("---End-------------\n")
	     }
	     result
    }).toList
    if (print)
    {
    println((headLine + "-------------------------------------------------------------------------------------------------").take(60))
    printGroupData(results,"Celková úspìšnost", "")
    printGroupData(results.filter(f => f._5 > 1).toList,"Úspìšnost v souvìtí ", "\t")
    
    results.groupBy(f => f._5).filterNot(f => f._1 < 2).map(f => f._1).toList.sorted.foreach(s => 
       printGroupData(results.filter(f => f._5 == s).toList,"Úspìšnost v souvìtí s " + s + " vìtami", "\t\t") 
    )
    printGroupData(results.filter(f => f._4).toList,"Úspìšnost ve vìtách s rozdìlenými klauzemi ", "\t")
    println(("end " +headLine + "-------------------------------------------------------------------------------------------------").take(60))
    }
    else 
    {
      // put in file
    }

    }
 
 def compareSentence(sentence : AnalyzedSentence , test : EstimateSentence ) : 
  (Boolean,Int,Int ,Boolean,Int) = 
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
       if (a.Level == t.level.getExactLevel) segmentsCompare(segments.tail,testSegments.tail,(acc._1 + 1, acc._2 ))
       else segmentsCompare(segments.tail,testSegments.tail,(acc._1 , acc._2 + 1 ))      }
    } 
   val compare = segmentsCompare(sentence.segments,test.getEstimateSegments,(0,0))
   (sentence.segments.length == test.getEstimateSegments.length,compare._1,compare._2, PackageInfo.containsGap(sentence),PackageInfo.isComplexSentence(sentence))
  }
 
 def printGroupData(data : List[ (Boolean,Int,Int , Boolean,Int)], headline : String , prefix : String) : Unit = 
 {
   println(createDataText(data, headline, prefix))
 }
 def createDataText(data : List[ (Boolean,Int,Int , Boolean,Int)], headline : String , prefix : String) : String = 
 {
    val wholeCount = data.map( a => a._2 + a._3).toList.sum
    val rightCount = data.map(a => a._2).toList.sum
    val wrongCount = data.map(a => a._3).toList.sum
    
    val rightCountSentence = data.filter(p => p._3 == 0).length 
    var builder = new StringBuffer
    
    builder.append(prefix +(headline + "-------------------------------------------------").take(50))
    builder.append("\n")
    builder.append(prefix +"Right Segments:")
    builder.append("\n")
    builder.append(prefix + (rightCount.doubleValue/wholeCount).toString )
  
    builder.append(prefix + rightCount.toString + " " + wholeCount.toString)
    builder.append("\n")
    builder.append(prefix + "Wrong Segments:")
    builder.append("\n")
    builder.append(prefix + wrongCount.doubleValue/wholeCount )
    
    builder.append(prefix + wrongCount.toString + " " + wholeCount.toString)
    builder.append("\n")
    builder.append(prefix + "Right Sentence:")
    builder.append("\n")
    builder.append(prefix + rightCountSentence.doubleValue/ data.length )
    builder.append("\n")
    builder.append(prefix + "Wrong Sentence:")
    builder.append("\n")
    builder.append(prefix + ((data.length - rightCountSentence).doubleValue/ data.length).toString)
    builder.append("\n")
    builder.append(prefix + data.length.toString)
    builder.append("\n")
    builder.append(prefix + ("end "+ headline + "---------------------------------------------").take(50))
    builder.toString
 }
}
 
