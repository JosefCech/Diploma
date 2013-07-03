package ClauseEstimate

import common.sentence.{ MorfSentence , ClauseSentence }
import common.segment.TaggedSegment
import common.{ Word }
import LevelEstimate.LevelAnalyzedSentence

class ClauseAnalyzedSentence(sentence : List[Word], ident : String ) 
   extends LevelAnalyzedSentence(sentence, ident)
   with ClauseSentence {

   def this (sentence : MorfSentence) =  this(sentence.morfWords,sentence.ident)
    
   def estimationOfClause : Int = {
        this.countEstimate(this.taggedSegments.map(t => t.analyzed).toList,false, false,false, 0)
    }
   
   def clauseEstimateSegments = this.estimateClauseNum(this.estimatedSegments.map(t => new TaggedSegment(t)).toList)
   
  val clause = clauseEstimateSegments.zipWithIndex.map(s => (s._2, s._1.clause)).toList.groupBy( f => f._2).map(f => (f._1, f._2.map(t => t._1)))
  val countOfClause = clause.map(f => f._1).toList.max
  
   override def toString =  this.clauseEstimateSegments.map(t => t.clause.toString + " " + t.level.toString +" "+t.getStartNewClause.toString + " \n ").toList.reduce(_ + _)
}