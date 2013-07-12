package common.segment

import common.{ AnalyzedWord , Word }

class AnalyzedSegment( val data : Segment,  level : Int , clause : Int,  startNewClause : Boolean ) 
	extends Segment {
 
  def words : List[Word] =  data.words
  
  override def toString =  "  level " + this.Level + "clause " + this.ClauseNum.toString + " segment : " + super.toString
  
  def Level : Int =   this.level
    
  super.level.setLevel(level)
  
  def ClauseNum = if (clause == -1) 0
                  else {
                    this.setClause(clause)
                    clause
                  }
  def Clausebeg = this.startNewClause
}