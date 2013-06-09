package common.sentence

import common.segment.AnalyzedSegment
import common.segment.Segment

class AnalyzedSentence(val segments : List[AnalyzedSegment]) extends Sentence(segments.map(t => t.words.map(f => f.word)).flatten)  {
  
  

}