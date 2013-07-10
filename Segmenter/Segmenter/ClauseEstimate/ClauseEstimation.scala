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


object ClauseEstimation extends App
{
  override def main(args: Array[String]) {
    def files = common.Directory.ReadAnxFiles(segmenter.Configuration.DataFolder("Develop")).toList
   var notGood = 0
   val pw = new java.io.PrintWriter(new File("logError"))
   val results = files.map(f => {
       val sentence = AnxReader.ReadAnalyzedSentence(f)
       val analyzed2 = new ClauseAnalyzedSentence(sentence.morfSentence)
       val analyzed1 = new ClauseAnalyzedSentence(sentence.sentenceWithLevel,sentence.Ident)
       // val matches = RuleHandler.rules.map( rule=>RuleAutomata.ruleMatches(analyzed.taggedSegments, rule)).toList.flatten
      //println(matches)
       val result = compareSentence(sentence.analyzedSentence,analyzed2)
    //   analyzed.addToLog("test")
       
       if (result._2 > 0)
       {
        // println(sentence)
       //  println(analyzed)
       //  println("-----celkem------------")
      //   println(analyzed.log)
     //     println("----konec------------")
         pw.write("---Start-----------\n")
        pw.write(sentence.toString)
        pw.write(analyzed2.toString)
        pw.write(analyzed2.log.toString)
        pw.write("---End-------------\n")
     
       }
        val countWithoutVerb = analyzed2.estimatedSegments.groupBy(f => f.clause).filterNot(p => p._2.filter(s => new InfoSegment(s).HaveActiveVerb).length > 0).filter(p =>  p._1 != 0).toList.length
         val existWithVerb =analyzed2.estimatedSegments.filter(p => new InfoSegment(p).HaveActiveVerb).length > 0  
       
       if (result._2 > 0 && countWithoutVerb > 0 && existWithVerb  )
       
       {
         pw.write(analyzed2.toString)
         pw.write(sentence.toString)
         pw.write(analyzed2.log.toString)
         pw.write(countWithoutVerb.toString + " / " + existWithVerb.toString)
         notGood +=1
       }
       result
   }).toList
    
    pw.close;
    
    println("not good tagged")
    println(notGood.doubleValue / results.length)
    val wholeCount = results.map( a => a._1 + a._2).toList.sum
    val rightCount = results.map(a => a._1).toList.sum
    val wrongCount = results.map(a => a._2).toList.sum
   
    val rightCountSentence = results.filter(p => p._2 == 0).length 
      
    println("Right Segments:")
    println(rightCount.doubleValue/wholeCount )
    println(rightCount.toString + " " + wholeCount.toString)
    println("Wrong Segments:")
    println(wrongCount.doubleValue/wholeCount )
    println(wrongCount.toString + " " + wholeCount.toString)
    
    println("Right Sentence:")
    println(rightCountSentence.doubleValue/ results.length )
    println("Wrong Sentence:")
    println((results.length - rightCountSentence).doubleValue/ results.length )
    println(results.length.toString)
   
   
    println("Estimated clause : ")
    println(results.map(f => f._4).toList.filter(p => p == 0).length.doubleValue / results.length)
    println("Estimated clause +- 1 :")
    println(results.map(f => f._4).toList.filter(p => p == 0 || p == 1 || p == -1).length.doubleValue / results.length)
    println("Estimated clause +- 2 : ")
    println(results.map(f => f._4).toList.filter(p => p <= 2 && p >= -2).length.doubleValue / results.length)
   
    println("Estimated clause +- 3 : ")
    println(results.map(f => f._4).toList.filter(p => p <= 3 && p >= -3).length.doubleValue / results.length)
    
     println("Estimated clause +- 4 : ")
    println(results.map(f => f._4).toList.filter(p => p <= 4 && p >= -4).length.doubleValue / results.length)
    
      println("Estimated clause +- 5 : ")
    println(results.map(f => f._4).toList.filter(p => p <= 5 && p >= -5).length.doubleValue / results.length)
    
        println("Estimated clause +- 6 : ")
    println(results.map(f => f._4).toList.filter(p => p <= 6 && p >= -6).length.doubleValue / results.length)
        println("Estimated clause +- 7 : ")
    println(results.map(f => f._4).toList.filter(p => p <= 7 && p >= -7).length.doubleValue / results.length)
    println("Estimated clause from result : ")
    println(results.map(f => f._5).toList.filter(p => p == 0).length.doubleValue / results.length)
    
    println("Estimated clause +- 1 : ")
    println(results.map(f => f._5).toList.filter(p => p <= 1 && p >= -1).length.doubleValue / results.length)
    
    println("Estimated clause +- 1 : ")
    println(results.map(f => f._5).toList.filter(p => p <= 2 && p >= -2).length.doubleValue / results.length)
   
    println("Estimated clause +- 3: ")
    println(results.map(f => f._5).toList.filter(p => p <= 3 && p >= -3).length.doubleValue / results.length)
    
    println("Estimated clause +- 4: ")
    println(results.map(f => f._5).toList.filter(p => p <= 4 && p >= -4).length.doubleValue / results.length)
    
    println("Estimated clause 0 - je spatne ? : ")
    println(results.map(f => (f._2 == 0,f._5)).toList.filter(p => p._2 == 0 && p._1 == false).length.doubleValue / results.map(f => (f._2 == 0,f._5)).toList.filter(p => p._2 == 0).length)
    
    val clauseData = results.map(f => (f._6._1,f._6._2,f._6._3,f._7)).toList
   
      
    println("Selected clause : ")
    println(clauseData.map(t => t._1).toList.sum.doubleValue / clauseData.map(t => t._1 + t._2 + t._3).sum)
    println("Wrong 1 - k existujicim klauzim segmenty navic")
    println(clauseData.map(t => t._2).toList.sum.doubleValue / clauseData.map(t => t._1 + t._2 + t._3).sum)
    println("Wrong 2 - spatny pocet klauzi")
    println(clauseData.map(t => t._3).toList.sum.doubleValue / clauseData.map(t => t._1 + t._2 + t._3).sum)
    
    println("Klauze s dirou : ")
    println(results.filter(p => p._7).length.doubleValue / results.length)
    println("Spravne urcene klauze s dirou : ")
    println(clauseData.filter(p => p._4).map(f => f._1).toList.sum.doubleValue / clauseData.filter(p => p._4).map(t => t._1 + t._2 + t._3).sum)
    println("Spravne urcene vetys s dirou : ")
    println(clauseData.filter(p => p._4 && p._2 == 0 && p._3 == 0).toList.length.doubleValue / clauseData.filter(p => p._4).length)
    
    println("Pomìr u chybových vìt")
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
 }
  
 def compareSentence(sentence : AnalyzedSentence , test : ClauseAnalyzedSentence ) : 
  (Int,Int , AnalyzedSentence, Int, Int,(Int,Int,Int), Boolean) = 
  {
   val compare = segmentsCompare(sentence.segments, test.clauseEstimateSegments,(0,0))
   val estimatedCount = {
            test.estimationOfClause - sentence.countClause
        }
   val estimationCountResult = {
        test.countOfClause - sentence.countClause //test.estimationOfClause

        
   }
   
   val estimatedClause = this.clauseCompare(sentence.clause.toList,test.clause.toList,(0,0,0))
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
