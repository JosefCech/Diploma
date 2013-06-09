package common.segment

import common.AnalyzedWord

class AnalyzedSegment(val words : List[AnalyzedWord], val startNewClause : Boolean,val level : Int ) extends ISegment {

}