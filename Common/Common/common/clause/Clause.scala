package common.clause

import common.segment.Segment
/** Clause - group of tuples (Index,Segment)
  * Base functions : 
  * 		add segment
  * 		merge clauses               
  * 
  * @constructor create clause from list of tuples (Index,Segment) and flag open
  * @constructor create clause from one tuple (Index,Segment) and flag - is set to true
  * @constructor create clause from list of tuples (Index,Segment) and flag - is set to true
  * @param segments - group of segments
  * @param segment - initial segment identificator  
  * @param open - flag - which tells if clause can accepted next segment or not   
  */
class Clause(val segments : List[(Int,Segment)], val open : Boolean ) {

  def this( segments : List[(Int,Segment)]) = this(segments,true)
  def this( segment : (Int,Segment)) = this(List(segment),true) 
  
  /** Add new segment with index */
   def addSegment(segment : Segment, num : Int) : Clause = {
     new Clause(((num,segment) :: this.segments))
   } 
   
  /** Concat two clauses */
   def mergeClause(clause : Clause) : Clause = {
      new Clause(clause.segments ::: this.segments,this.open)
   }
   
   /** Get tuples in right order*/
   def getSegments = segments.reverse
   
   /** Closing clause */
   def close : Clause = new Clause(this.segments,false)
   
   /** information of setting flag "open" */
   def isOpen : Boolean = open
   
   
}