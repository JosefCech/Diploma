package common.segment

import common.{ AnalyzedWord , Word }

class AnalyzedSegment( val data : Segment,  level : Int , clause : Int,  startNewClause : Boolean ) 
	extends Segment {
 
  def words : List[Word] =  data.words
  
  override def toString = " level , clause " + this.Level.toString + ", " + this.ClauseNum.toString + " " + this.Clausebeg.toString+ "\t\t" + super.toString
  
  def Level = this.level
  def ClauseNum = this.clause
  def Clausebeg = this.startNewClause
}