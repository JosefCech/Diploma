package ClauseEstimate

import StatisticModul.StatisticClauseEstimate
import Anx.AnxReader
import java.io.{ File, PrintWriter}
import common.sentence.AnalyzedSentence
import DataObjects.EstimateSentence
import common.segment.AnalyzedSegment
import common.segment.Segment
import common.segment.InfoSegment
import Translation._
import java.io.PrintWriter
import common.sentence.AnxSentence
import StatisticModul.StatisticLevelEstimate
import LevelEstimate.LevelAnalyzedSentence
import DataObjects.ResultsClause
import DataObjects.ResultsClause
import Xml.Writer

object ClauseStatisticEstimate extends App{

   override def main(args: Array[String]) {
	/*var dataFolder : String = "Test"
  	if (args.size > 0)
  	{
  		dataFolder = args.apply(0)
  	}
  	
	val withAnotatedLevel : Boolean = args.apply(1) match {
		case "-l" => true
		case _ => false
	}*/
	
  	List("Develop","Heldout", "Test", "TestClause").foreach(dataFolder => {
  		  List(true,false).foreach( withLevel => {
		   def files = common.Directory.ReadAnxFiles(segmenter.Configuration.DataFolder(dataFolder)).toList
		   def resultOutput = segmenter.Configuration.ResultFolder(dataFolder) + "/results-" + dataFolder ;
		   val resultfile = new PrintWriter(new File("resultOutput"))
		   val pw = new PrintWriter(new File("logErrorLevel"))
		   this.analyzedData(files,dataFolder, pw, withLevel)
  		  })
  	})
 }
 
  def analyzedData(files : List[File], dataSet : String,  pw : PrintWriter, withLevel : Boolean) : Unit  = 
  {
  
   val statisticClauseModel = new StatisticClauseEstimate
   val statisticLevelModel = new StatisticLevelEstimate
   var notGoodAnalyzed = 0
   var notGoodSimple = 0
   val results = files.map(f => {
		     val sentence = AnxReader.ReadAnalyzedSentence(f)
		     if (withLevel) {
		     val analyzed = statisticClauseModel.StatisticEstimateClause(sentence)
		     val result = ClauseEstimation.compareSentence(sentence.analyzedSentence, analyzed)
		     this.writeResultIntoLog(pw, sentence, analyzed, result) 
		      result
		     }
		     else {
		     //val analyzed = statisticLevelModel.StatisticEstimateLevel(sentence)
		     val analyzed = new LevelAnalyzedSentence(sentence.morfSentence)
		     val analyzed2 = statisticClauseModel.StatisticEstimateClause(analyzed, sentence.Ident)
		     val result = ClauseEstimation.compareSentence(sentence.analyzedSentence, analyzed2)
		     this.writeResultIntoLog(pw, sentence, analyzed, result)
		     result
		     }
   }).toList
    
    pw.close;
   
   	val resultsData = new ResultsClause(results)
   	withLevel match {
   		case true => Writer.Write("Result/"+"StatisticClause-" + dataSet + "-withLevel.xml", resultsData)
   		case false => Writer.Write("Result/"+"StatisticClause-" + dataSet + "-withoutLevel.xml", resultsData)
   	}
   	 println(this.createResultData(results, "Level annotated data - whole data", ""))
    println(this.createResultData(results.filter(p => p._5), "Level annotated data - with break", "\t"))
    println(this.createResultData(results.filter(p => p._6 > 1), "Level annotated data -complex sentence", "\t"))
    
    val resultfile = new java.io.PrintWriter(new File("results1"))
    resultfile.write(this.createResultData(results, "Level annotated data - whole data", ""))
    resultfile.write(this.createResultData(results.filter(p => p._5), "Level annotated data - with break", "\t"))
    resultfile.write(this.createResultData(results.filter(p => p._6 > 1), "Level annotated data -complex sentence", "\t"))
    
    
    
    val maxCountSegmentInResult =  results.map(p => p._3.countClause).max
    resultfile.write("Results data --------------------------------------------------------------------------------------------------------------------\n")
    var i = 1
    while (i < maxCountSegmentInResult + 1)
    {
      resultfile.write("Count " +i + "\n")
      resultfile.write(this.createResultData(results.filter(f =>  f._3.countClause <= i && i - 1 < f._3.countClause ), "Level no annotated data - whole data", ""))
       resultfile.write(" end Count " +i + "\n")
     i +=1
    }
    resultfile.write("End Results data --------------------------------------------------------------------------------------------------------------------\n")
   
    resultfile.write("Results  simple data --------------------------------------------------------------------------------------------------------------------\n")
    resultfile.write("End Results  simple data --------------------------------------------------------------------------------------------------------------------\n")
  
    resultfile.close
  }
   
 def compareSentence(sentence : AnalyzedSentence , test : EstimateSentence ) : 
  (Int,Int , AnalyzedSentence, Int, Int,(Int,Int,Int), Boolean) = 
  {
   val compare = segmentsCompare(sentence.segments, test.getEstimateSegments,(0,0))
   
   val estimatedCount = {
            test.getEstimationOfCountClause - sentence.countClause
        }
   val estimationCountResult = {
        test.getCountOfClause - sentence.countClause //test.estimationOfClause

        
   }
   
   val estimatedClause = this.clauseCompare(sentence.clause.toList,test.getClause.toList,(0,0,0))
   val containsBreakClause = !sentence.clause.filter(p => containsGap(p._2.sorted)).filter(f => f._1 != 0).isEmpty
  // if (containsBreakClause) println(sentence.clause)
   (compare._1,compare._2, sentence, estimatedCount, estimationCountResult,estimatedClause, containsBreakClause)
  }
  
  private def containsGap(data : List[Int]) : Boolean =
  {
    if (data.length == 1){
      false
    } 
    else if (data.head + 1 == data.tail.head) {
      containsGap(data.tail)
    }
    else {
      true
    }
  }
  def segmentsCompare(segments : List[AnalyzedSegment], testSegments : List[Segment], acc : (Int,Int)) : (Int, Int) = 
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
       if (a.ClauseNum == t.clause) segmentsCompare(segments.tail,testSegments.tail,(acc._1 + 1, acc._2 ))
       else segmentsCompare(segments.tail,testSegments.tail,(acc._1 , acc._2 + 1 ))      
       }
    }
  
  def clauseCompare(analyzedSentence : List[(Int,List[Int])], testSentence : List[(Int,List[Int])], acc : (Int,Int,Int)  )  : (Int,Int,Int) = 
  {
     def compareSegmentList(a : List[Int], t : List[Int]) : Boolean =
     {
       if (a.isEmpty && t.isEmpty) true
       else if (a.isEmpty || t.isEmpty) false
       else if (a.head == t.head) compareSegmentList(a.tail,t.tail)
       else false
     }
     
    if (analyzedSentence.isEmpty || testSentence.isEmpty)
    {
      var wrong1 : Int = acc._2
      var wrong2 : Int = acc._3
      if (analyzedSentence.isEmpty) wrong1 += testSentence.length
      else if (testSentence.isEmpty) wrong2 += analyzedSentence.length
      
      (acc._1,wrong1,wrong2)
    }
    else 
    {
      val headA = analyzedSentence.head
      val dataTest = testSentence.filter(p => p._1 == headA._1 )
      if (dataTest.isEmpty)
      {
        clauseCompare(analyzedSentence.tail, testSentence.filterNot(p => p._1 == headA._1).toList,(acc._1,acc._2,acc._3+1))
      }
      else 
      {
        val headT = dataTest.head
        if (compareSegmentList(headA._2.sorted, headT._2.sorted))
        {
           clauseCompare(analyzedSentence.tail, testSentence.filterNot(p => p._1 == headA._1).toList,(acc._1+1,acc._2,acc._3))
        }
        else
        {
           clauseCompare(analyzedSentence.tail, testSentence.filterNot(p => p._1 == headA._1).toList,(acc._1,acc._2 + 1 ,acc._3))
        }
      }
    }
  }
  
 def writeResultIntoLog(pw: PrintWriter, sentence: AnxSentence, analyzed : EstimateSentence, result: (Int, Int, common.sentence.AnalyzedSentence, (Int, Int, Int), Boolean, Int)) =
 {
	 if (result._2 > 0)
	 {
	  pw.write("---Start-----------\n")
	  pw.write(sentence.toString)
	  pw.write(analyzed.toString)
	  pw.write("---End-------------\n")
	 }
	 val countWithoutVerb = analyzed.getEstimateSegments.groupBy(f => f.clause).filterNot(p => p._2.filter(s => new InfoSegment(s).HaveActiveVerb).length > 0).filter(p =>  p._1 != 0).toList.length
     val existWithVerb =analyzed.getEstimateSegments.filter(p => new InfoSegment(p).HaveActiveVerb).length > 0  
      if (result._2 > 0 && countWithoutVerb > 0 && existWithVerb  )
      {	pw.write(analyzed.toString)
    	pw.write(sentence.toString)
        pw.write(countWithoutVerb.toString + " / " + existWithVerb.toString)
        //notGoodAnalyzed +=1
      }
      pw.write("Simple Data result")
 }
  def createResultData(data : List[(Int,Int , AnalyzedSentence, (Int,Int,Int), Boolean,Int)], headLine : String , prefix : String) : String =
  {
    val wholeCount = data.map( a => a._1 + a._2).toList.sum
    val rightCount = data.map(a => a._1).toList.sum
    val wrongCount = data.map(a => a._2).toList.sum
   
    val rightCountSentence = data.filter(p => p._2 == 0).length 
    
     val clauseData = data.map(f => (f._4._1,f._4._2,f._4._3,f._5)).toList
    
    var builder = new StringBuilder
    builder.append(prefix + (headLine + "-------------------------------------------------------------------------------------------------").take(60))
    builder.append("\n")
    builder.append(prefix + "Right Segments:")
    builder.append("\n")
    builder.append(prefix + rightCount.doubleValue/wholeCount )
    builder.append("\n")
    builder.append(prefix + rightCount.toString + " " + wholeCount.toString)
    builder.append("\n")
    builder.append(prefix + "Wrong Segments:")
    builder.append("\n")
    builder.append(prefix + wrongCount.doubleValue/wholeCount )
    builder.append("\n")
    builder.append(prefix + wrongCount.toString + " " + wholeCount.toString)
    builder.append("\n")
    builder.append(prefix + "Right selected clause : ")
    builder.append("\n")
    builder.append(prefix + clauseData.map(t => t._1).toList.sum.doubleValue / clauseData.map(t => t._1 + t._2 + t._3).sum)
    builder.append("\n")
    builder.append(prefix + "Wrong 1 - k existujicim klauzim segmenty navic")
    builder.append("\n")
    builder.append(prefix + clauseData.map(t => t._2).toList.sum.doubleValue / clauseData.map(t => t._1 + t._2 + t._3).sum)
    builder.append("\n")
    builder.append(prefix + "Wrong 2 - spatny pocet klauzi")
    builder.append("\n")
    builder.append(prefix + clauseData.map(t => t._3).toList.sum.doubleValue / clauseData.map(t => t._1 + t._2 + t._3).sum)
    builder.append("\n")
    builder.append(prefix + "Right Sentence:")
    builder.append("\n")
    builder.append(prefix + rightCountSentence.doubleValue/ data.length )
    builder.append("\n")
    builder.append(prefix + "Wrong Sentence:")
    builder.append("\n")
    builder.append(prefix + (data.length - rightCountSentence).doubleValue/ data.length )
    builder.append("\n")
    builder.append(prefix + data.length.toString)
    builder.append("\n")
     builder.append(prefix +("end" + headLine + "-------------------------------------------------------------------------------------------------").take(60))
  
    builder.toString
  }
def writeToFile(p: String, s: String) {
    val pw = new java.io.PrintWriter(new File(p))
    try {
      pw.write(s)
    } finally {
      pw.close()
    }
  }
}
