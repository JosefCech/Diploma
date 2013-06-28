package common.segment

import common.{ AnalyzedWord , Word }

class AnalyzedSegment(val data : List[AnalyzedWord], level : Int , clause: Int, startNewClause : Boolean ) 
	extends Segment {

  
  def words : List[Word] =
			 data.map( f => f match 
		  					{
		  						case f : AnalyzedWord => f.word
		  					}
		  			)	
}