package ClauseEstimate

import common.sentence.{ MorfSentence , ClauseSentence }
import common.segment.TaggedSegment
import common.{ Word }
import LevelEstimate.LevelAnalyzedSentence
import Rules.MatchEffect

class ClauseAnalyzedSentence(sentence : List[Word], ident : String ) 
   extends LevelAnalyzedSentence(sentence, ident)
   with ClauseSentence {

   def this (sentence : MorfSentence) =  this(sentence.morfWords,sentence.ident)
    
   def estimationOfClause : Int = {
        this.countEstimate(this.taggedSegments.map(t => t.analyzed).toList,false, false,false, 0)
    }
   
    var clauseEstimateSegments = this.estimateClauseNum(this.estimatedSegments.map(t => new TaggedSegment(t)).toList)
   
  val clause = clauseEstimateSegments.zipWithIndex.map(s => (s._2, s._1.clause)).toList.groupBy( f => f._2).map(f => (f._1, f._2.map(t => t._1)))
  val countOfClause = clause.map(f => f._1).toList.max
  
  override def toString =  this.clauseEstimateSegments.map(t => t.clause.toString + " " + t.level.toString +" "+t.getStartNewClause.toString + " \n ").toList.reduce(_ + _)

  override def addToLog(s : String) : Unit = {
      if (this.log == null) this.log = ""
      this.log += s
     // println(this.log)
      this.log+= "\n"
    }
  
  
   
  def applyMatches(matches : List[MatchEffect] ) = {
     this.clauseEstimateSegments.zipWithIndex.foreach(f => {
       val matchEffect = matches.filter(p => p.effectOnIndex == f._2)
       if (!matchEffect.isEmpty) {
         f._1.setClause(matchEffect.head.clauseNum)
       }
     })  
  }
}