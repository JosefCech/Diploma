package common.sentence

import common.segment.AnalyzedSegment
import common.segment.Segment
import common.segment.PureSegment
import common.segment.Boundary

class AnalyzedSentence(val Ident : String,  data : List[Segment]) 
	extends Sentence(data.map(t => t.words.map(f => f)).flatten)  {
  
  val segments  = data.map( s => s match { case s : AnalyzedSegment  => s
  										   case s : PureSegment => new AnalyzedSegment(s,s.level.getExactLevel,s.clause,s.getStartNewClause)
  										   case s : Boundary => new AnalyzedSegment(s,s.level.getExactLevel,0,s.getStartNewClause)
  										}
		  				
		  				) ;
  
  val clause = segments.zipWithIndex.map(s => (s._2, s._1.ClauseNum)).toList.groupBy( f => f._2).map(f => (f._1, f._2.map(t => t._1)))
  val countClause = clause.map(f => f._1).toList.max
   
  
  override def toString =super.toString + "\n" + this.segments.map(s => s.ClauseNum.toString+ " " + s.LevelDefault.toString  + "\n").toList.reduce(_ + _ )
}