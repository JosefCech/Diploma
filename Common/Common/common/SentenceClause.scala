package common

class SentenceClause(clauses : List[(Int,List[Clause])]) {

   def addNewClause(clause : Clause, level : Int ) : SentenceClause = {
     def clauses = this.clauses.filter(t => t._1 == level).last._2
     def openClauses = clauses.filter(p => p.isOpen)
     if (openClauses.isEmpty)
     {
       def newClause = List[Clause](clause)
       new SentenceClause(this.clauses ::: List[(Int,List[Clause])]((level,newClause))) 
     }
     else {
         def closed = clauses.filterNot(p => p.isOpen)
         def open = openClauses.head   
         
         def newClause = List[Clause](this.mergeClauses(open,clause))
         
         new SentenceClause(this.clauses.filterNot(t => t._1 == level) ::: List[(Int,List[Clause])]((level,closed ::: newClause)))
     }
   } 
   
  
   def mergeClauses(open : Clause , newClause : Clause) : Clause = {  
     open.mergeClause(newClause)
   }
   
   
}