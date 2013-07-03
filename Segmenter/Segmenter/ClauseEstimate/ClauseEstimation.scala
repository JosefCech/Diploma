package ClauseEstimate

import Anx.AnxReader
import common.sentence.{AnalyzedSentence }
import common.segment.{AnalyzedSegment, Segment}

object ClauseEstimation extends App
{
  override def main(args: Array[String]) {
    def files = common.Directory.ReadAnxFiles(segmenter.Configuration.DataFolder("Develop")).toList
    
   val results = files.map(f => {
       val sentence = AnxReader.ReadAnalyzedSentence(f)
       val analyzed = new ClauseAnalyzedSentence(sentence.morfSentence)
       val result = compareSentence(sentence.analyzedSentence,analyzed)
       if (result._2 > 0)
       {
         println(sentence)
         println(analyzed)
        // println(result)
       }
       result
   }).toList
    
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
    println("Estimated clause - mensi nebo rovno : ")
    println(results.map(f => f._4).toList.filter(p => p == 0 || p == 1).length.doubleValue / results.length)
    println("Estimated clause - mensi nebo rovno : ")
    println(results.map(f => f._4).toList.filter(p => p == -1).length.doubleValue / results.length)
 
    println("Estimated clause from result : ")
    println(results.map(f => f._5).toList.filter(p => p == 0).length.doubleValue / results.length)
    println("Estimated clause - mensi nebo rovno : ")
    println(results.map(f => f._5).toList.filter(p => p == 0 || p == 1).length.doubleValue / results.length)
    println("Estimated clause - mensi nebo rovno : ")
    println(results.map(f => f._5).toList.filter(p => p == -1).length.doubleValue / results.length)
    
    val clauseData = results.map(f => f._6).toList
   
      
    println("Selected clause : ")
    println(clauseData.map(t => t._1).toList.sum.doubleValue / clauseData.map(t => t._1 + t._2 + t._3).sum)
    println("Wrong 1 - k existujicim klauzim segmenty navic")
    println(clauseData.map(t => t._2).toList.sum.doubleValue / clauseData.map(t => t._1 + t._2 + t._3).sum)
    println("Wrong 2 - spatny pocet klauzi")
    println(clauseData.map(t => t._3).toList.sum.doubleValue / clauseData.map(t => t._1 + t._2 + t._3).sum)
 }
  
 def compareSentence(sentence : AnalyzedSentence , test : ClauseAnalyzedSentence ) : 
  (Int,Int , AnalyzedSentence, Int, Int,(Int,Int,Int)) = 
  {
   val compare = segmentsCompare(sentence.segments, test.clauseEstimateSegments,(0,0))
   val estimatedCount = {
          if (sentence.countClause > test.estimationOfClause)
          {
            -1
          }
          else if (sentence.countClause < test.estimationOfClause)
          {
            1
          }
          else 
          {
            0
          }
        }
   val estimationCountResult = {
        if (sentence.countClause > test.countOfClause)
          {
            -1
          }
          else if (sentence.countClause < test.countOfClause)
          {
            1
          }
          else 
          {
            0
          }
        
   }
   
   val estimatedClause = this.clauseCompare(sentence.clause.toList,test.clause.toList,(0,0,0))
   (compare._1,compare._2, sentence, estimatedCount, estimationCountResult,estimatedClause)
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
}
