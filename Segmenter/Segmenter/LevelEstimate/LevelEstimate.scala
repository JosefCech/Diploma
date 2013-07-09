package LevelEstimate

import Anx.AnxReader
import common.sentence.{ LevelEstimateSentence, AnalyzedSentence }
import common.segment.{ Segment, AnalyzedSegment}

object LevelEstimate extends App {
 override def main(args: Array[String]) {
    def files = common.Directory.ReadAnxFiles(segmenter.Configuration.DataFolder("Develop")).toList
    
   val results = files.map(f => {
     val sentence = AnxReader.ReadAnalyzedSentence(f)
     val analyzed = new LevelAnalyzedSentence(sentence.morfSentence)
    // println(sentence.Ident)
    val result = compareSentence(sentence.analyzedSentence, analyzed)
    if (result._3 > 0)
      
    {
    
    // println(analyzed)
    // println(sentence)
    // println(result)
    }
    
    result
    }).toList
    
    val wholeCount = results.map( a => a._2 + a._3).toList.sum
    val rightCount = results.map(a => a._2).toList.sum
    val wrongCount = results.map(a => a._3).toList.sum
   
    val rightCountSentence = results.filter(p => p._3 == 0).length 
      
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
    
 }
 def compareSentence(sentence : AnalyzedSentence , test : LevelAnalyzedSentence ) : 
  (Boolean,Int,Int , AnalyzedSentence) = 
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
   val compare = segmentsCompare(sentence.segments,test.estimatedSegments,(0,0))
   (sentence.segments.length == test.estimatedSegments.length,compare._1,compare._2, sentence)
  }
 
 }
 
