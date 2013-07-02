package ClauseEstimate

import common.sentence.{ MorfSentence , ClauseSentence }
import common.{ Word }

class LevelAnalyzedSentence(sentence : List[Word], ident : String  
    with ClauseSentence {

   def this (sentence : MorfSentence) =  this(sentence.morfWords,sentence.ident)
    
   def estimationOfClause : Int = {
        this.countEstimate(this.taggedSegments.map(t => t.analyzed).toList,false, false,false, 0)
    }
   
   def estimatedSegments = this.estimateLevelSegments(this.segments)
}