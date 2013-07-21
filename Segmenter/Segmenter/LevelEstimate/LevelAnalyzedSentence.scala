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
  
   override val getEstimationOfCountClause : Int = 0
   override val getCountOfClause :Int = 0
   override val getClause : Map[Int,List[Int]] = Map[Int,List[Int]]()
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
   override val getEstimationOfCountClause : Int = 0
   override val getCountOfClause :Int = 0
   override val getClause : Map[Int,List[Int]] = Map[Int,List[Int]]()
 
}

class LevelStatisticAnalyzedSentenceDiff(sentence : AnxSentence, path : List[Int] )
	extends  EstimateSentence
{
  
   
  var estimatedSegments = this.applyBestPath(sentence,path)
  
  def applyBestPath(sentence : AnxSentence, path : List[Int]) : List[Segment] = {
   
    def applyGuess(segments : List[Segment], guesses:List[Int], actual : Int, acc : List[Segment]) : List[Segment] = {
      if (segments.isEmpty) acc.reverse
      else {
        val headSegment = segments.head
        headSegment.setLevel(actual + guesses.head)
        applyGuess(segments.tail,guesses.tail,actual + guesses.head, headSegment :: acc)
      }
    }
     applyGuess(sentence.segments,path,0, List[Segment]())
 }    
  
  
    
  override def toString =  this.estimatedSegments.map(t => t.level.toString + " " + t.getStartNewClause.toString + " \n ").toList.reduce(_ + _)
  
  override val getEstimateSegments = this.estimatedSegments
   override val getEstimationOfCountClause : Int = 0
   override val getCountOfClause :Int = 0
   override val getClause : Map[Int,List[Int]] = Map[Int,List[Int]]()
 
}