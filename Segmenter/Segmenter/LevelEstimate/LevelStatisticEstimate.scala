package LevelEstimate

import Anx.AnxReader
import common.sentence.{ LevelEstimateSentence, AnalyzedSentence }
import common.segment.{ Segment, AnalyzedSegment}
import java.io.File
import DataObjects.EstimateSentence
import StatisticModul.StatisticLevelEstimate
import StatisticModul.StatisticLevelEstimateDiff


object LevelStatisticEstimate extends App {
  
  override def main(args: Array[String]) {
    val dataSets = List("Develop","Heldout","Test")
    val printData =true
    dataSets.foreach(dataSet => this.analyzedSetData(dataSet, dataSet + "-file", "\t", printData))
   
 }
 
 def analyzedSetData(dataSet : String, headLine : String, prefix : String , print : Boolean) : Unit =
 {
    def files = common.Directory.ReadAnxFiles(segmenter.Configuration.DataFolder(dataSet)).toList
    // log data chybne ur�en� v�ty
    val pw = new java.io.PrintWriter(new File("logErrorLevel" + dataSet))
    val statisticModel = new StatisticLevelEstimate
    val results = files.map(f => {
	     val sentence = AnxReader.ReadAnalyzedSentence(f)
	     val analyzed = statisticModel.StatisticEstimateLevel(sentence)
	     //val statisticModelDiff = new StatisticLevelDiffEstimate
	     val result = LevelEstimate.compareSentence(sentence.analyzedSentence, analyzed)
	     
	     if (result._3 > 0)
	     {
	        pw.write("---Start-----------\n")
	        pw.write(sentence.toString)
	        pw.write(analyzed.toString)
	     //   pw.write(analyzed.getLog)
	        pw.write("---End-------------\n")
     }
     result
    }).toList
    
    if (print)
    {
    println((headLine + "-------------------------------------------------------------------------------------------------").take(60))
    LevelEstimate.printGroupData(results,"Celkov� �sp�nost", "")
     LevelEstimate.printGroupData(results.filter(f => f._5 > 1).toList,"�sp�nost v souv�t� ", "\t")
    
    results.groupBy(f => f._5).filterNot(f => f._1 < 2).map(f => f._1).toList.sorted.foreach(s => 
     LevelEstimate.printGroupData(results.filter(f => f._5 == s).toList,"�sp�nost v souv�t� s " + s + " v�tami", "\t\t") 
    )
     LevelEstimate.printGroupData(results.filter(f => f._4).toList,"�sp�nost ve v�t�ch s rozd�len�mi klauzemi ", "\t")
    println(("end " +headLine + "-------------------------------------------------------------------------------------------------").take(60))
    }
    else 
    {
      // put in file
    }

    }
  
}