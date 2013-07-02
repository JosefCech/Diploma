package ClauseEstimate

import Anx.AnxReader
import common.sentence.{AnalyzedSentence }
import common.segment.{AnalyzedSegment, Segment}

object ClauseEstimation extends App {
 override def main(args: Array[String]) {
    def files = common.Directory.ReadAnxFiles(segmenter.Configuration.DataFolder("Develop")).toList
    
   val results = files.map(f => {
       val sentence = AnxReader.ReadAnalyzedSentence(f)
       val analyzed = new ClauseAnalyzedSentence(sentence.morfSentence)
       null
   })
 }
 
  def compareSentence(sentence : AnalyzedSentence , test : ClauseAnalyzedSentence ) : 
  (Int,Int , AnalyzedSentence) = 
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
       if (a.ClauseNum == t.clause) segmentsCompare(segments.tail,testSegments.tail,(acc._1 + 1, acc._2 ))
       else segmentsCompare(segments.tail,testSegments.tail,(acc._1 , acc._2 + 1 ))      }
    } 
   val compare = segmentsCompare(sentence.segments, test.clauseSegments,(0,0))
   (compare._1,compare._2, sentence)
  }
}