package LevelEstimate

import Anx.AnxReader
import common.sentence.{ LevelEstimateSentence, AnalyzedSentence }
import common.segment.{ Segment, AnalyzedSegment}
import java.io.File
import DataObjects.EstimateSentence
import StatisticModul.StatisticLevelEstimate
import StatisticModul.StatisticLevelEstimateDiff
import Translation.Translation


object LevelStatisticEstimate extends App {
  
  override def main(args: Array[String]) {
    val dataSets = List("Develop","Heldout","Test")
    val printData =true
    dataSets.foreach(dataSet => this.analyzedSetData(dataSet, dataSet + "-file", "\t", printData))
    }
 
 def analyzedSetData(dataSet : String, headLine : String, prefix : String , print : Boolean) : Unit =
 {
    def files = common.Directory.ReadAnxFiles(segmenter.Configuration.DataFolder(dataSet)).toList
    // log data chybně určené věty
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
	        pw.write("---End-------------\n")
     }
     result
    }).toList
    
    if (print)
    {
    println((headLine + "-------------------------------------------------------------------------------------------------").take(60))
    LevelEstimate.printGroupData(results,Translation.succesTotal, "")
    LevelEstimate.printGroupData(results.filter(f => f._5 > 1).toList,Translation.succesComplex, "\t")
    
    results.groupBy(f => f._5).filterNot(f => f._1 < 2).map(f => f._1).toList.sorted.foreach(s => 
     LevelEstimate.printGroupData(results.filter(f => f._5 == s).toList,Translation.succesComplexWith(s), "\t\t") 
    )
     LevelEstimate.printGroupData(results.filter(f => f._4).toList,Translation.succesSplitSentence, "\t")
    println(("end " +headLine + "-------------------------------------------------------------------------------------------------").take(60))
    }
    else 
    {
      //TODO put in file
    }

    }
  
}