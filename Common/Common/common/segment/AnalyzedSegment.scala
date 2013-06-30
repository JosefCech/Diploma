package common.segment

import common.{ AnalyzedWord , Word }

class AnalyzedSegment( data : Segment,  level : Int , clause : Int,  startNewClause : Boolean ) 
	extends Segment {
 
  def words : List[Word] =  data.words
  
  override def toString = " level , clause " + this.level.toString + ", " + this.clause.toString + " " 
  
  def Level = this.level
  def ClauseNum = this.clause
  def Clauseberg = this.startNewClause
}