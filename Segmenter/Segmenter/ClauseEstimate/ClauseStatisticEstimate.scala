package ClauseEstimate

import StatisticModul.StatisticClauseEstimate
import Anx.AnxReader
import java.io.File
import common.sentence.AnalyzedSentence
import DataObjects.EstimateSentence
import common.segment.AnalyzedSegment
import common.segment.Segment
import common.segment.InfoSegment


object ClauseStatisticEstimate extends App{

   override def main(args: Array[String]) {
 
   def files = common.Directory.ReadAnxFiles(segmenter.Configuration.DataFolder("Test")).toList
     val pw = new java.io.PrintWriter(new File("logErrorLevel"))
     val statisticModel = new StatisticClauseEstimate
   //   val statisticModel = new StatisticClauseEstimate
      var notGoodAnalyzed = 0
      var notGoodSimple = 0
     val results = files.map(f => {
     val sentence = AnxReader.ReadAnalyzedSentence(f)
     val analyzed2 = statisticModel.StatisticEstimateClause(sentence)
     val result = ClauseEstimation.compareSentence(sentence.analyzedSentence, analyzed2)
     //println(result)

       if (result._2 > 0)
       {
        
       //  println("-----celkem------------")
      //   println(analyzed.log)
     //     println("----konec------------")
         pw.write("---Start-----------\n")
        pw.write(sentence.toString)
        pw.write(analyzed2.toString)
       // pw.write(analyzed2.getLog)
        pw.write("---End-------------\n")
     
       }
        val countWithoutVerb = analyzed2.getEstimateSegments.groupBy(f => f.clause).filterNot(p => p._2.filter(s => new InfoSegment(s).HaveActiveVerb).length > 0).filter(p =>  p._1 != 0).toList.length
         val existWithVerb =analyzed2.getEstimateSegments.filter(p => new InfoSegment(p).HaveActiveVerb).length > 0  
       
       if (result._2 > 0 && countWithoutVerb > 0 && existWithVerb  )
       
       {
         pw.write(analyzed2.toString)
         pw.write(sentence.toString)
        // pw.write(analyzed2.log.toString)
         pw.write(countWithoutVerb.toString + " / " + existWithVerb.toString)
           notGoodAnalyzed +=1
       }
       result
   }).toList
    
     pw.write("Simple Data result")
       
        pw.write("Simple Data result")
   val resultssimple = files.map(f => {
       val sentence = AnxReader.ReadAnalyzedSentence(f)
       val analyzed2 = statisticModel.StatisticEstimateClause(sentence)
       // porovnej naètená a analyzovaná data
       val result = ClauseEstimation.compareSentence(sentence.analyzedSentence,analyzed2)
 
       // zápis chybových vìt do logu       
       if (result._2 > 0)
       {
        pw.write("---Start-----------\n")
        pw.write(sentence.toString)
        pw.write(analyzed2.toString)
       
        pw.write("---End-------------\n")
     
       }
       // poèet klauzí bez slovesa
        val countWithoutVerb = analyzed2.getEstimateSegments.groupBy(f => f.clause).filterNot(p => p._2.filter(s => new InfoSegment(s).HaveActiveVerb).length > 0).filter(p =>  p._1 != 0).toList.length
        // existuje nìjaké sloveso
        val existWithVerb =analyzed2.getEstimateSegments.filter(p => new InfoSegment(p).HaveActiveVerb).length > 0  
       
       if (result._2 > 0 && countWithoutVerb > 0 && existWithVerb  )       
       {
         // vìta porušuje konzistenci z pohledu sloves a klauzí 
         pw.write(countWithoutVerb.toString + " / " + existWithVerb.toString)
         notGoodSimple +=1
       }
       result
   }).toList
   
    pw.close;
    
  
    println("not good tagged")
    println(notGoodAnalyzed.doubleValue / results.length)
    println(notGoodSimple.doubleValue / results.length)
    println(ClauseEstimation.createResultData(results, "Level annotated data - whole data", ""))
    println(ClauseEstimation.createResultData(results.filter(p => p._5), "Level annotated data - with break", "\t"))
    println(ClauseEstimation.createResultData(results.filter(p => p._6 > 1), "Level annotated data -complex sentence", "\t"))
    println(ClauseEstimation.createResultData(resultssimple, "Level no annotated data- whole", ""))
    println(ClauseEstimation.createResultData(resultssimple.filter(p => p._5), "Level no annotated data-clause with break", "\t"))
    println(ClauseEstimation.createResultData(resultssimple.filter(p => p._6 > 1), "Level no annotated data-compex sentence", "\t"))
    val resultfile = new java.io.PrintWriter(new File("results1"))
    val maxCountSegmentInResult =  results.map(p => p._3.countClause).max
    val maxCountSegmentInResultSimple =  resultssimple.map(p => p._3.countClause).max
    resultfile.write("Results data --------------------------------------------------------------------------------------------------------------------\n")
    var i = 1
    while (i < maxCountSegmentInResult + 1)
    {
      resultfile.write("Count " +i + "\n")
      resultfile.write(ClauseEstimation.createResultData(results.filter(f =>  f._3.countClause <= i && i - 1 < f._3.countClause ), "Level no annotated data - whole data", ""))
       resultfile.write(" end Count " +i + "\n")
     i +=1
    }
    resultfile.write("End Results data --------------------------------------------------------------------------------------------------------------------\n")
   
    resultfile.write("Results  simple data --------------------------------------------------------------------------------------------------------------------\n")
  
    i = 1
    while (i < maxCountSegmentInResultSimple + 1)
    {
      resultfile.write("Count " +i + "\n")
      resultfile.write(ClauseEstimation.createResultData(resultssimple.filter(f => f._3.countClause <= i && i - 1 < f._3.countClause ), "Level no annotated data - whole data", ""))
       resultfile.write(" end Count " +i + "\n")
     i +=1
    }
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

def writeToFile(p: String, s: String) {
    val pw = new java.io.PrintWriter(new File(p))
    try {
      pw.write(s)
    } finally {
      pw.close()
    }
  }
}
