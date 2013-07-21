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
    // log data chybne urèené vìty
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
    LevelEstimate.printGroupData(results,"Celková úspìšnost", "")
     LevelEstimate.printGroupData(results.filter(f => f._5 > 1).toList,"Úspìšnost v souvìtí ", "\t")
    
    results.groupBy(f => f._5).filterNot(f => f._1 < 2).map(f => f._1).toList.sorted.foreach(s => 
     LevelEstimate.printGroupData(results.filter(f => f._5 == s).toList,"Úspìšnost v souvìtí s " + s + " vìtami", "\t\t") 
    )
     LevelEstimate.printGroupData(results.filter(f => f._4).toList,"Úspìšnost ve vìtách s rozdìlenými klauzemi ", "\t")
    println(("end " +headLine + "-------------------------------------------------------------------------------------------------").take(60))
    }
    else 
    {
      // put in file
    }

    }
  
}