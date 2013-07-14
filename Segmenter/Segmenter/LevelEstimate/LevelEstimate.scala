package LevelEstimate

import Anx.AnxReader
import common.sentence.{ LevelEstimateSentence, AnalyzedSentence }
import common.segment.{ Segment, AnalyzedSegment}
import java.io.File

object LevelEstimate extends App {
  
 override def main(args: Array[String]) {
    // naètení odhadovaných dat
    def files = common.Directory.ReadAnxFiles(segmenter.Configuration.DataFolder("Develop")).toList
    // log data chybne urèené vìty
    val pw = new java.io.PrintWriter(new File("logErrorLevel"))
    val results = files.map(f => {
        // naètení vìty
	    val sentence = AnxReader.ReadAnalyzedSentence(f)
	    // analýza vìty bez naètených informací
	    val analyzed = new LevelAnalyzedSentence(sentence.morfSentence)
	    // porovnání naètené a analyzované vìty
	    //(stejný poèet segmentù,poèet správnì urèených segmentù , poèet špatnì urèených segmentù , celá vìta)
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
    
    printGroupData(results,"Celková úspìšnost", "")
    /*
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
    */
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
 
 
 def printGroupData(data : List[ (Boolean,Int,Int , AnalyzedSentence)], headline : String , prefix : String) : Unit = 
 {
    val wholeCount = data.map( a => a._2 + a._3).toList.sum
    val rightCount = data.map(a => a._2).toList.sum
    val wrongCount = data.map(a => a._3).toList.sum
    
    val rightCountSentence = data.filter(p => p._3 == 0).length 
    
    println("-----------------" + headline + "-------------------")
    println(prefix +"Right Segments:")
    println(prefix + (rightCount.doubleValue/wholeCount).toString )
    println(prefix + rightCount.toString + " " + wholeCount.toString)
    println(prefix + "Wrong Segments:")
    println(prefix + wrongCount.doubleValue/wholeCount )
    println(prefix + wrongCount.toString + " " + wholeCount.toString)
    
    println(prefix + "Right Sentence:")
    println(prefix + rightCountSentence.doubleValue/ data.length )
    println(prefix + "Wrong Sentence:")
    println(prefix + ((data.length - rightCountSentence).doubleValue/ data.length).toString)
    println(prefix + data.length.toString)
    println("-----------------end "+ headline + "-------------------")
 }
}
 
