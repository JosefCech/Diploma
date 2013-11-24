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
import Translation.Translation
import common.sentence.LevelEstimateSentence
import DataObjects.Estimation
import DataObjects.ResultsClause
import DataObjects.ResultsClause
import Xml.Writer


object ClauseEstimation extends App
{
  override def main(args: Array[String]) {

  	List("Develop","Heldout", "Test", "TestClause").foreach(dataSet => {
    def files = common.Directory.ReadAnxFiles(segmenter.Configuration.DataFolder(dataSet)).toList
   var notGoodAnalyzed = 0
   var notGoodSimple = 0
   val pw = new java.io.PrintWriter(new File("logError"))
   pw.write("Analyzed Data result")
   val results = files.map(f => {
       val sentence = AnxReader.ReadAnalyzedSentence(f)
       //val analyzed2 = new ClauseAnalyzedSentence(sentence.Words, sentence.Ident, sentence.sentenceWithLevel)
       
       val analyzed = new ClauseAnalyzedSentence(sentence.sentenceWithLevel,sentence.Ident)
       
         //(stejný počet segmenů, počet správně určených segmentů , počet  špatně určených segmentů , rozštěpení klauze, počet vět)
       val result = compareSentence(sentence.analyzedSentence,analyzed)
       Estimation.writeSentence(analyzed, dataSet, "ruleClauseEstimation")
       // z�pis chybov�ch v�t do logu       
       if (result._2 > 0)
       {
        pw.write("---Start-----------\n")
        pw.write(sentence.toString)
        pw.write(analyzed.toString)
        pw.write(analyzed.getLog)
        pw.write("---End-------------\n")
     
       }
       // po�et klauz� bez slovesa
        val countWithoutVerb = analyzed.estimatedSegments.groupBy(f => f.clause).filterNot(p => p._2.filter(s => new InfoSegment(s).HaveActiveVerb).length > 0).filter(p =>  p._1 != 0).toList.length
        // existuje n�jak� sloveso
        val existWithVerb =analyzed.estimatedSegments.filter(p => new InfoSegment(p).HaveActiveVerb).length > 0  
       
       if (result._2 > 0 && countWithoutVerb > 0 && existWithVerb  )       
       {
         // v�ta poru�uje konzistenci z pohledu sloves a klauz� 
         pw.write(countWithoutVerb.toString + " / " + existWithVerb.toString)
         notGoodAnalyzed +=1
       }
       result
   }).toList
   val resultsData = new ResultsClause(results)
   resultsData.print(notGoodAnalyzed, notGoodSimple)
   Writer.Write("Result/"+"RuleClause-" + dataSet + "-withLevel.xml", resultsData)
    
   notGoodAnalyzed = 0
   notGoodSimple = 0
   
   pw.write("Simple Data result")
   val resultssimple = files.map(f => {
       val sentence = AnxReader.ReadAnalyzedSentence(f)
       val analyzed2 = new ClauseAnalyzedSentence(sentence.morfSentence)
       //Estimation.writeSentence(analyzed, dataSet, "ruleClauseEstimation")
       // porovnej na�ten� a analyzovan� data
       val result = compareSentence(sentence.analyzedSentence,analyzed2)
 
       // z�pis chybov�ch v�t do logu       
       if (result._2 > 0)
       {
        pw.write("---Start-----------\n")
        pw.write(sentence.toString)
        pw.write(analyzed2.toString)
        pw.write(analyzed2.getLog)
        pw.write("---End-------------\n")
     
       }
       // po�et klauz� bez slovesa
        val countWithoutVerb = analyzed2.estimatedSegments.groupBy(f => f.clause).filterNot(p => p._2.filter(s => new InfoSegment(s).HaveActiveVerb).length > 0).filter(p =>  p._1 != 0).toList.length
        // existuje n�jak� sloveso
        val existWithVerb =analyzed2.estimatedSegments.filter(p => new InfoSegment(p).HaveActiveVerb).length > 0  
       
       if (result._2 > 0 && countWithoutVerb > 0 && existWithVerb  )       
       {
         // veta porusuje konzistenci z pohledu sloves a klauzi 
         pw.write(countWithoutVerb.toString + " / " + existWithVerb.toString)
         notGoodSimple +=1
       }
       result
   }).toList
    
    pw.close;
    
   val resultsDataSimple = new ResultsClause(resultssimple)
   resultsDataSimple.print(notGoodAnalyzed, notGoodSimple)
   Writer.Write("Result/"+"RuleClause-" + dataSet + "-withoutLevel.xml", resultsDataSimple)
  	}
  	)
}
 
  
 def compareSentence(sentence : AnalyzedSentence , test : EstimateSentence) : 
  (Int,Int , AnalyzedSentence, (Int,Int,Int), Boolean, Int) = 
  {
   // porovn�n� jednotliv�ch segment�
   val compare = segmentsCompare(sentence.segments, test.getEstimateSegments,(0,0))
  // println(sentence.segments)
   //println(test.getEstimateSegments)
   
   val estimatedClause = this.clauseCompare(sentence.clause.toList,test.getClause.toList,(0,0,0))
   val containsBreakClause = PackageInfo.containsGap(sentence)
   val isComplexSentence = PackageInfo.isComplexSentence(sentence)
  // if (containsBreakClause) println(sentence.clause)
   (compare._1,compare._2, sentence, estimatedClause, containsBreakClause, isComplexSentence)
  }
  
  
  def segmentsCompare(segments : List[AnalyzedSegment], testSegments : List[Segment], acc : (Int,Int)) : (Int, Int) = 
    {
      if (segments.isEmpty || testSegments.isEmpty)
      {
        // nenalezene ��dn� segmenty => jsou ozna�eny za chybu
        var wrong = 0;
        if (segments.isEmpty) wrong += testSegments.length
        if (testSegments.isEmpty) wrong += segments.length
        (acc._1,acc._2 + wrong)
      }
      else 
      {
       val a = segments.head 
       val t = testSegments.head
       val aClause =  a.ClauseNum
       val tClause = t.clause
      
       if (a.ClauseNum == t.clause)
       { // spr�vn� za�azen� segment do klauze
         segmentsCompare(segments.tail,testSegments.tail,(acc._1 + 1, acc._2 ))
       }
       else
       { // �patn� za�azen� segment
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
