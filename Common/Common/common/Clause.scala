package common

class Clause(val segments : List[(Int,Segment)], val open : Boolean = true) {

   def addSegment(segment : Segment, num : Int) : Clause = {
     new Clause(((num,segment) :: this.segments).reverse)
   } 
   
   def mergeClause(clause : Clause) : Clause = {
      new Clause(this.segments ::: clause.segments,this.open)
   }
   
   def close : Clause = new Clause(this.segments,false)
   
   def isOpen = open
   
   
}