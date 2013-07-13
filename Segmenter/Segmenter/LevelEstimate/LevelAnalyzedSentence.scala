package LevelEstimate

import common.sentence.{ MorfSentence, LevelEstimateSentence, Sentence }
import common.Word
import DataObjects.EstimateSentence
import common.sentence.AnxSentence
import Anx.AnxReader
import common.segment.Segment

class LevelAnalyzedSentence(sentence : List[Word], ident : String  )
	extends MorfSentence(sentence, ident ) with LevelEstimateSentence with EstimateSentence
{
  def this (sentence : MorfSentence) =  this(sentence.morfWords,sentence.ident)
   
  var estimatedSegments = this.estimateLevelSegments(this.segments)
  override def toString =  this.estimatedSegments.map(t => t.level.toString + " " + t.getStartNewClause.toString + " \n ").toList.reduce(_ + _)
  
  override val getEstimateSegments = this.estimatedSegments
}

class LevelStatisticAnalyzedSentence(sentence : AnxSentence, path : List[Int] )
	extends  EstimateSentence
{
  
   
  var estimatedSegments = this.applyBestPath(sentence,path)
  
  def applyBestPath(sentence : AnxSentence, path : List[Int]) : List[Segment] = {
   
     def applyBestPath(sentence : List[Segment], path : List[Int], acc : List[Segment]) : List[Segment] = 
     {
       if (sentence.isEmpty)
       {
         acc.reverse
       }
       else
       {       
         val newSegment = sentence.head
         newSegment.setLevel(path.head)
         applyBestPath(sentence.tail, path.tail, newSegment :: acc)
       }
     }    
     applyBestPath(sentence.segments, path, List[Segment]())
  }
  
    
  override def toString =  this.estimatedSegments.map(t => t.level.toString + " " + t.getStartNewClause.toString + " \n ").toList.reduce(_ + _)
  
  override val getEstimateSegments = this.estimatedSegments
}