package ClauseEstimate

import Anx.AnxReader
import common.sentence.{AnalyzedSentence }
import common.segment.{AnalyzedSegment, Segment}
import java.io.File
import Rules.RuleHelper
import Rules.RuleAutomata
import common.segment.InfoSegment
import common.sentence.Sentence
import common.segment.TaggedSegment
import DataStatistic.PackageInfo
import DataObjects.EstimateSentence


object ClauseEstimation extends App
{
  override def main(args: Array[String]) {
    def files = common.Directory.ReadAnxFiles(segmenter.Configuration.DataFolder("Test")).toList
   var notGoodAnalyzed = 0
   var notGoodSimple = 0
   val pw = new java.io.PrintWriter(new File("logError"))
   pw.write("Analyzed Data result")
   val results = files.map(f => {
       val sentence = AnxReader.ReadAnalyzedSentence(f)
       val analyzed2 = new ClauseAnalyzedSentence(sentence.sentenceWithLevel,sentence.Ident)
       // porovnej naètená a analyzovaná data
       val result = compareSentence(sentence.analyzedSentence,analyzed2)
 
       // zápis chybových vìt do logu       
       if (result._2 > 0)
       {
        pw.write("---Start-----------\n")
        pw.write(sentence.toString)
        pw.write(analyzed2.toString)
        pw.write(analyzed2.getLog)
        pw.write("---End-------------\n")
     
       }
       // poèet klauzí bez slovesa
        val countWithoutVerb = analyzed2.estimatedSegments.groupBy(f => f.clause).filterNot(p => p._2.filter(s => new InfoSegment(s).HaveActiveVerb).length > 0).filter(p =>  p._1 != 0).toList.length
        // existuje nìjaké sloveso
        val existWithVerb =analyzed2.estimatedSegments.filter(p => new InfoSegment(p).HaveActiveVerb).length > 0  
       
       if (result._2 > 0 && countWithoutVerb > 0 && existWithVerb  )       
       {
         // vìta porušuje konzistenci z pohledu sloves a klauzí 
         pw.write(countWithoutVerb.toString + " / " + existWithVerb.toString)
         notGoodAnalyzed +=1
       }
       result
   }).toList
   
    pw.write("Simple Data result")
   val resultssimple = files.map(f => {
       val sentence = AnxReader.ReadAnalyzedSentence(f)
       val analyzed2 = new ClauseAnalyzedSentence(sentence.morfSentence)
       // porovnej naètená a analyzovaná data
       val result = compareSentence(sentence.analyzedSentence,analyzed2)
 
       // zápis chybových vìt do logu       
       if (result._2 > 0)
       {
        pw.write("---Start-----------\n")
        pw.write(sentence.toString)
        pw.write(analyzed2.toString)
        pw.write(analyzed2.getLog)
        pw.write("---End-------------\n")
     
       }
       // poèet klauzí bez slovesa
        val countWithoutVerb = analyzed2.estimatedSegments.groupBy(f => f.clause).filterNot(p => p._2.filter(s => new InfoSegment(s).HaveActiveVerb).length > 0).filter(p =>  p._1 != 0).toList.length
        // existuje nìjaké sloveso
        val existWithVerb =analyzed2.estimatedSegments.filter(p => new InfoSegment(p).HaveActiveVerb).length > 0  
       
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
    println(this.createResultData(results, "Level annotated data - whole data", ""))
    println(this.createResultData(results.filter(p => p._5), "Level annotated data - with break", "\t"))
    println(this.createResultData(results.filter(p => p._6 > 1), "Level annotated data -complex sentence", "\t"))
    println(this.createResultData(resultssimple, "Level no annotated data- whole", ""))
    println(this.createResultData(resultssimple.filter(p => p._5), "Level no annotated data-clause with break", "\t"))
    println(this.createResultData(resultssimple.filter(p => p._6 > 1), "Level no annotated data-compex sentence", "\t"))
    val resultfile = new java.io.PrintWriter(new File("results1"))
    val maxCountSegmentInResult =  results.map(p => p._3.countClause).max
    val maxCountSegmentInResultSimple =  resultssimple.map(p => p._3.countClause).max
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
  
    i = 1
    while (i < maxCountSegmentInResultSimple + 1)
    {
      resultfile.write("Count " +i + "\n")
      resultfile.write(this.createResultData(resultssimple.filter(f => f._3.countClause <= i && i - 1 < f._3.countClause ), "Level no annotated data - whole data", ""))
       resultfile.write(" end Count " +i + "\n")
     i +=1
    }
     resultfile.write("End Results  simple data --------------------------------------------------------------------------------------------------------------------\n")
  
    resultfile.close
    /* 
    val rightCountError = results.filter(a => a._2 > 0).map(a => a._1).toList.sum
    val wrongCountError = results.filter(a => a._2 > 0).map(a => a._2).toList.sum
    println(wrongCountError / rightCountError.toDouble)
   val max  = 55
   var i :  Int = 10
    while ( i < max)
    {
    val wholeCount10 = results.filter(a => a._1 + a._2 > i ).map( a => a._1 + a._2).toList.sum
    val rightCount10 = results.filter(a => a._1 + a._2 > i ).map(a => a._1).toList.sum
    val wrongCount10 = results.filter(a => a._1 + a._2 > i ).map(a => a._2).toList.sum
    println("Right Segments:" + i.toString)
    println(rightCount10.doubleValue/wholeCount10 )
    println(rightCount10.toString + " " + wholeCount10.toString)
    println("Wrong Segments:")
    println(wrongCount10.doubleValue/wholeCount10 )
    println(wrongCount10.toString + " " + wholeCount10.toString)
    i+=5
    }
    */
 }
  
 def compareSentence(sentence : AnalyzedSentence , test : EstimateSentence) : 
  (Int,Int , AnalyzedSentence, (Int,Int,Int), Boolean, Int) = 
  {
   // porovnání jednotlivých segmentù
   val compare = segmentsCompare(sentence.segments, test.getEstimateSegments,(0,0))
  // println(sentence.segments)
   //println(test.getEstimateSegments)
   
   val estimatedClause = this.clauseCompare(sentence.clause.toList,test.getClause.toList,(0,0,0))
   val containsBreakClause = PackageInfo.containsGap(sentence)
   val isComplexSentence = PackageInfo.isComplexSentence(sentence)
  // if (containsBreakClause) println(sentence.clause)
   (compare._1,compare._2, sentence, estimatedClause, containsBreakClause, isComplexSentence)
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
  def segmentsCompare(segments : List[AnalyzedSegment], testSegments : List[Segment], acc : (Int,Int)) : (Int, Int) = 
    {
      if (segments.isEmpty || testSegments.isEmpty)
      {
        // nenalezene žádné segmenty => jsou oznaèeny za chybu
        var wrong = 0;
        if (segments.isEmpty) wrong += testSegments.length
        if (testSegments.isEmpty) wrong += segments.length
        (acc._1,acc._2 + wrong)
      }
      else 
      {
       val a = segments.head 
       val t = testSegments.head
       if (a.ClauseNum == t.clause)
       { // správnì zaøazený segment do klauze
         segmentsCompare(segments.tail,testSegments.tail,(acc._1 + 1, acc._2 ))
       }
       else
       { // špatnì zaøazený segment
         segmentsCompare(segments.tail,testSegments.tail,(acc._1 , acc._2 + 1 ))      
       }
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
