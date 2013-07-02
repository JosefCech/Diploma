package ClauseEstimate

import common.sentence.{ MorfSentence , ClauseSentence }
import common.{ Word }

class ClauseAnalyzedSentence(sentence : List[Word], ident : String  )
	extends MorfSentence(sentence, ident )
    with ClauseSentence {

   def estimationOfClause : Int = {
        this.countEstimate(this.taggedSegments.map(t => t.analyzed).toList,false, false,false, 0)
    }
   
}