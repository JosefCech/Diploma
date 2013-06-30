package common.sentence

import common.segment.AnalyzedSegment
import common.segment.Segment

class AnalyzedSentence(val Ident : String,  data : List[Segment]) 
	extends Sentence(data.map(t => t.words.map(f => f)).flatten)  {
  
  val segments  = data.map( s => s match { case s : AnalyzedSegment  => s } ) ;
  

}