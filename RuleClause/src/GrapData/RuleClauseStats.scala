package GrapData

import common.Directory
import java.io.File
import Pdt.MorfReader
import readersAndWriters.SegReader
import Rules.AnalyzedSentence

object RuleClauseStats extends App {
   def goldenDataFolder : String = "../GoldenData"
   def segmentFolder : String = RuleClauseStats.goldenDataFolder + "/Seg" 
   def pdtFolder : String = RuleClauseStats.goldenDataFolder + "/Morf"
   def resultFolder : String = RuleClauseStats.goldenDataFolder + "/Result"
   
 
  
  override def main(args: Array[String]) {
   val segFilesAll : List[File] = Directory.ReadSegFiles(RuleClauseStats.segmentFolder )
   val morfFilesAll : List[File] = Directory.ReadPdtMorfFiles(RuleClauseStats.pdtFolder)
   val segSentences = segFilesAll.map( f =>   ("m-" + f.getName.split('.').apply(0),f))
  
   val segFiles = segSentences.map( s => {
		    val segArray = s._1.split("-")
		  segArray.apply(1) + "_" + segArray.apply(2)
		   }).distinct
   
    val filteredFiles  =  morfFilesAll.map(t => (t.getName.split('.').apply(0),t)).filter(t => segFiles.contains(t._1)) 	   
    
    val sentences =  filteredFiles.map(t => MorfReader.Read(t._2).toList).toList.flatten.map(p =>
      { 
        val data = segSentences.filter(s => s._1 == p.ident).toList
        if (data.isEmpty){
         (p, Array[(Int,Int)]())
        }
        else{
          (p,SegReader.ReadData(data.head._2)._2)
        }
      }).toList.filterNot(_._2.isEmpty).toList
                     
    println(sentences.filter(p => p._1.segments.size == p._2.size).size)
    println(sentences.filter(p => p._1.segments.size != p._2.size).size)
    
    val zeroStepAnalyze = sentences.map(r => (r._1,new AnalyzedSentence(r._1.segments,r._1.ident),r._2))
    
    val firstStepAnalyze = sentences.map(r => (r._1,new AnalyzedSentence(r._1.segments,r._1.ident).getLevels,r._2))
    printProblemData(zeroStepAnalyze)
    printData(firstStepAnalyze)
    
   }
   
   def printProblemData(firstStepAnalyze : List[(common.Sentence,AnalyzedSentence, Array[(Int, Int)])]) : Unit = {
     println("Wrong analyze data")
     val data = firstStepAnalyze.filter(p => p._2.isForTesting);
     var rightData = data.filter(p => p._2.levelConfiguration == p._3.foldLeft("")((r,a) => r + a._1.toString))
     var wrongData = data.filterNot(p => p._2.levelConfiguration == p._3.foldLeft("")((r,a) => r + a._1.toString))
     println("Whole data: "+data.size+" Right: " + rightData.size + " wrong: " + wrongData.size)
     
     wrongData.foreach(f => {
        println("Ident : " + f._1.ident + " Estimation: " + f._2.levelConfiguration + " Origin: " + f._3.foldLeft("")((r,a) => r + a._1.toString))
     })
   }
   
   def printData(firstStepAnalyze : List[(common.Sentence, List[Int], Array[(Int, Int)])]) : Unit = {
    
    println("Exact match sentence level")
    var right_analyze = firstStepAnalyze.filter(p => p._2.foldLeft("")((s,l) => s + l) == p._3.foldLeft("")((s,l) => s + l._1)).size
    var wrong_analyze = firstStepAnalyze.filter(p => p._2.foldLeft("")((s,l) => s + l) != p._3.foldLeft("")((s,l) => s + l._1)).size
    var count_sentence = firstStepAnalyze.size
    
    println((right_analyze.toFloat/count_sentence) * 100)
    println((wrong_analyze.toFloat/count_sentence) * 100)
    println(firstStepAnalyze.filter(p => p._2.foldLeft("")((s,l) => s + l) != p._3.foldLeft("")((s,l) => s + l._1)).size)
    
   
    var count_segments = firstStepAnalyze.foldLeft(0)((a,b) => a + b._2.size)
    println("Match per segment")
   var right_analyze_segment = firstStepAnalyze.foldLeft(0)((a,b) => {
       def countMatch(estimationLevels : List[Int], foundLevels : List[(Int,Int)] ,  countMatchAcc : Int) : Int = {
        if (estimationLevels.isEmpty || foundLevels.isEmpty) {
          countMatchAcc
        }
        else {
          if (estimationLevels.head == foundLevels.head._1){
            val newCount = countMatchAcc + 1
            countMatch(estimationLevels.tail, foundLevels.tail, newCount)
         }
         else {
             countMatch(estimationLevels.tail, foundLevels.tail, countMatchAcc)
           }
         }
        }
        a + countMatch(b._2,b._3.toList,0)
       }
    )
   var wrong_analyze_segment = count_segments - right_analyze_segment
    
    println((right_analyze_segment.toFloat/count_segments)*100)
    println((wrong_analyze_segment.toFloat/count_segments)*100)
    
   val right_count_clause =  firstStepAnalyze.filter(p => p._1.estimationOfClause == p._3.filter(t => t._2 == 0).size).size
   println((right_count_clause.floatValue /count_sentence) * 100)
    
   val wrong_count_clause =  firstStepAnalyze.filter(p => p._1.estimationOfClause != p._3.filter(t => t._2 == 0).size)
   .map(p => (p._1.ident, p._1.estimationOfClause,p._3.filter(t => t._2 == 0).size))
   
   
   val almost1 = wrong_count_clause.filter(p => p._2 + 1 == p._3).size
     println("Almost + 1")
     println((almost1.toFloat /count_sentence) * 100)
  val almost2 = wrong_count_clause.filter(p => p._2 == p._3 + 1).size     
     println("Almost -1 ")
     println((almost2.toFloat /count_sentence) * 100)
     
   println(wrong_count_clause.filter(p => p._2 == p._3 + 1))
   }
   // def sentences = args.map(t => this.parsedSegments(t)).toList.flatten
}
