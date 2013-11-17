package common.segment

import common.{ AnalyzedWord , Word }

class AnalyzedSegment( val data : Segment,  level : Int , clause : Int,  startNewClause : Boolean ) 
	extends Segment {
 
  def this(data:Segment) = this(data,data.level.getExactLevel, data.clause, data.getStartNewClause)
  def words : List[Word] =  data.words
  
  override def toString =  "  level " + this.LevelDefault + "clause " + this.ClauseNum.toString + " segment : " + super.toString
  
  def LevelDefault : Int =   level
   
  super.level = new Level(level,level)
  
  def ClauseNum = if (clause == -1) 0
                  else {
                    this.setClause(clause)
                    clause
                  }
  def Clausebeg = this.startNewClause
  
  def taggedSegment : TaggedSegment = {  
     new TaggedSegment(data)   
  }
  
  def getInfoSegment : InfoSegment = new InfoSegment(this.data)
}
