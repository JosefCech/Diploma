package common.sentence

import common.segment.AnalyzedSegment
import common.segment.Segment

class AnalyzedSentence(val Ident : String,  data : List[Segment]) 
	extends Sentence(data.map(t => t.words.map(f => f)).flatten)  {
  
  val segments  = data.map( s => s match { case s : AnalyzedSegment  => s } ) ;
  
  val clause = segments.zipWithIndex.map(s => (s._2, s._1.ClauseNum)).toList.groupBy( f => f._2).map(f => (f._1, f._2.map(t => t._1)))
  val countClause = clause.map(f => f._1).toList.max
   
  
  override def toString =super.toString + "\n" + this.segments.map(s => s.ClauseNum.toString+ " " + s.Level.toString  + "\n").toList.reduce(_ + _ )
}