package LevelEstimate

import common.sentence.{ MorfSentence, LevelEstimateSentence, Sentence }
import common.Word

class LevelAnalyzedSentence(sentence : List[Word], ident : String  )
	extends MorfSentence(sentence, ident ) with LevelEstimateSentence
{
  def this (sentence : MorfSentence) =  this(sentence.morfWords,sentence.ident)
   
  var estimatedSegments = this.estimateLevelSegments(this.segments)
  override def toString =  this.estimatedSegments.map(t => t.level.toString + " " + t.getStartNewClause.toString + " \n ").toList.reduce(_ + _)
}