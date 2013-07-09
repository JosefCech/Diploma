package common.sentence

import common.segment.{ InfoSegment , TaggedSegment, Segment }
import common.Tag
import common.segment.AnalyzedSegment
import common.clause.Clause
import common.clause.Clause


trait ClauseSentence {

 private var clauses : List[(Int,Clause)] = List[(Int,Clause)]() ;
 private var lastClause : Int = 0
  var log : String = ""
 def addToLog(text : String) : Unit

 private def maxClauseNum =
   if (clauses.isEmpty) {
     -1
   }
   else{
     clauses.map(f => f._1).max
   }
 
 private def tryAddSegment(c :  Int, s : Segment) : Boolean = {
   if (clauses.isEmpty || this.maxClauseNum < c) {
     true
   }
   else {
     val clausesSelected : List[(Int,Clause)] = clauses.filter(p => p._1 == c).toList
     if (!clausesSelected.isEmpty)
     {
      val clause =  clausesSelected.head._2
      clause.tryAddSegment(s)
     }
     else{
       true
     }
   }
 }
 
 private def addSegment(c : Int , i : Int, s : Segment ) : Unit = 
 {
    def getPreviousClause(actual : Int) : Int =
	 {
	   val data = this.clauses.filter(p => p._2.level <= actual && p._2.isOpen).toList.sortBy(f => f._1).toList.reverse
	   this.addToLog("previous")
	   this.addToLog(data.toString)
	   if (data.isEmpty) {
	     0
	   }
	   else {
	     data.head._1
	   }
	 }
 
    def getSuperiorClause(actual : Int) : Int = 
  {  
   val data = this.clauses.filter(p => p._2.level < actual && p._2.isOpen).toList.filter(p => p._1 != 0).sortBy(f => f._1).toList.reverse
    this.addToLog("superior")
	this.addToLog(data.toString)
   if (data.isEmpty){
     getPreviousClause(actual)
   }
   else{
     data.head._1
   } 
 }
   if (this.clauses.filter( f => f._1 == c).length > 0){
   // println("addClause" + c.toString)
       clauses.filter(p => p._1 == c).head._2.addSegment(i, s)
   }
   else {
     val actualLevel = s.level.getExactLevel
     val previous = getPreviousClause(actualLevel)
     val superior = getSuperiorClause(actualLevel)
      this.addToLog("create " + c.toString)
    this.addToLog(previous.toString + "/" + superior.toString + " / "+ actualLevel.toString)
     val newClauses = (c, new Clause(List[(Int,Segment)]((i,s)),true)) :: this.clauses
     this.clauses = newClauses; 
     this.clauses.head._2.setPreviousClause(previous)
     this.clauses.head._2.setSuperiorClause(superior)
   }
   this.lastClause = c
 }
 
 private def getPreviousClause(actualLevel : Int) : Int =
 {
   val data = this.clauses.filter(p => p._1 == actualLevel)
   if (data.isEmpty) {
     0
   }
   else {
     data.head._2.previous
   }
 }
 
 private def getSuperiorClause(actualLevel : Int) : Int = 
 {  
   val data = this.clauses.filter(p => p._1 == actualLevel)
   if (data.isEmpty) {
     0
   }
   else {
     data.head._2.superior
   }
 }
 
 private def closeClause(c : Int) : Unit = {
   if (c <= this.lastClause)
   {
     this.clauses.filter(c1 => c1._1 == c).head._2.close
   }
 }
 
private  def getNextClause = 
  if (this.clauses.isEmpty) 1
  else clauses.map(p => p._1).toList.max + 1

private def setNullBoundaryClause(acc : List[Segment]) : Unit = {
   if (new InfoSegment(acc.head).IsBoundarySegment) {
     acc.head.setClause(0)
   }
 }
 /** Estimation count of clause based on active verbs */
 def countEstimate(segments : List[InfoSegment], inBracket : Boolean , countBoundary : Boolean, haveVerb : Boolean , acc : Int) : Int = {
      if (segments.isEmpty){
        
    	  if (haveVerb) {
    	    acc
    	  }
    	  else{
    	    acc + 1
    	  }
      }
      else {
        if (!inBracket && segments.head.HaveOpeningBracket.size > 0) {
         
           countEstimate(segments.tail,true,countBoundary,haveVerb,acc + 1)
        }
        else if ((!inBracket) && segments.head.HaveCloseBracket.size > 0) {
         
          countEstimate(segments.tail,false,countBoundary,haveVerb,acc + 1)
        }
        else {
        val new_acc : Int = {
         if (countBoundary && segments.head.IsBoundarySegment ){
           if (segments.tail.isEmpty) {
             acc + 1
           }
           else if (segments.tail.head.IsBoundarySegment){
             acc
           }
           else {
             acc +1
           }
         }
         else if (!segments.head.IsBoundarySegment && segments.head.HaveActiveVerb){
           acc + 1
         }
         else if (segments.tail.isEmpty && segments.head.IsBoundarySegment && !haveVerb) {
           acc + 1
         }
         else {
           acc
         }
       }
   
        if (segments.head.IsBoundarySegment) {
          
          countEstimate(segments.tail,inBracket,countBoundary,haveVerb,new_acc)
        }
        else if (segments.head.HaveActiveVerb) {
          countEstimate(segments.tail,false,true,true,new_acc)
        }
        else {
          countEstimate(segments.tail,false,false,haveVerb,new_acc)
        }
        
       }
      }
     }
  
 def estimateMaxClause(segments : List[Segment]) : Int = segments.map(t => t.clause).toList.max
   
 def estimateClauseNum(segments : List[TaggedSegment]) : List[Segment] = {
   
    def estimateClauseNum(segments : List[(TaggedSegment,Int)], previousTag : String, actualClause : Int, acc:List[Segment]) : List[Segment] = {
      if (segments.isEmpty) {
        // all claused segments
        acc.reverse
      }
      else {
        val head = segments.head
        val headTag = head._1.GetTag
        val segment = head._1
        val index = head._2
        if (headTag.isBoundary && !acc.isEmpty)
        {
       this.addToLog( "add 1 :" + index.toString )
          segment.segment.setClause(0)
          this.addSegment(0, index, segment.segment)
          estimateClauseNum(segments.tail,headTag.tag, actualClause, segment.segment :: acc)
        }
        else if (headTag.isBoundary && 
                 headTag.compare(5, "Z") && 
                 acc.isEmpty
                )
        {
       addToLog("add 2:" + index.toString )
          segment.segment.setClause(0)
          this.addSegment(0, index, segment.segment)         
          estimateClauseNum(segments.tail,headTag.tag, actualClause, segment.segment :: acc)
        }        
        else 
       {
         
          val prevTag : Tag = new Tag(previousTag)
          val prevPureSegment : Tag = getPreviousSegmentTag(acc);
              
          val nextClause = {
               this.getNextClause
          }
          
          if ((headTag.haveSubflag ) && !acc.isEmpty && !prevTag.haveCordConj)
          {
             this.addToLog("add 3 :" + index.toString)
             this.setNullBoundaryClause(acc)                      
            
            // aktualni segment ma subflag a ma alespon jeden predchozi segment
            this.addSegment(nextClause, index, segment.segment)
            segment.segment.setClause(nextClause)
            estimateClauseNum(segments.tail,headTag.tag, nextClause, segment.segment :: acc)         
          }
          else if (prevTag.isBoundary &&  headTag.Level == prevPureSegment.Level)
          {
          
        
            // predchozi segment byla carka nebo jednoducha spojka
            val addToClauseSegment = this.tryAddSegment(actualClause, segment.segment)
          
           
            if (addToClauseSegment)
            {
             this.addToLog("add 4 :" + index.toString + segment.segment.toString)
             acc.head.setClause(actualClause)
             this.addSegment(actualClause, index, segment.segment)
             segment.segment.setClause(actualClause)
             this.updateClause(actualClause, acc)
             estimateClauseNum(segments.tail,headTag.tag, actualClause, segment.segment :: acc)
            }
            else {
               this.addToLog("add 5:" + index.toString)
              this.closeClause(actualClause)
               this.setNullBoundaryClause(acc)     
               segment.segment.setClause(nextClause)
              this.addSegment(nextClause, index, segment.segment)
             estimateClauseNum(segments.tail,headTag.tag, nextClause, segment.segment :: acc)
            }
          }                
          else if ((headTag.Level < prevPureSegment.Level && !prevPureSegment.isEmpty) )
          {
            
            val superiorClause = this.getSuperiorClause(actualClause)
         this.addToLog("superior clause " + superiorClause.toString)
            val addToClauseSegment = this.tryAddSegment(superiorClause, segment.segment)
             this.setNullBoundaryClause(acc)      
            if (addToClauseSegment){
              this.addToLog("add 6 :" + index.toString)
              this.closeClause(actualClause)
              this.addSegment(superiorClause, index, segment.segment)
             segment.segment.setClause(superiorClause)
             estimateClauseNum(segments.tail,headTag.tag, superiorClause, segment.segment :: acc)
            }
            else{
               this.addToLog("add 7 :" + index.toString)
              this.closeClause(actualClause)
              this.addSegment(nextClause, index, segment.segment)
             segment.segment.setClause(nextClause)
             estimateClauseNum(segments.tail,headTag.tag, nextClause, segment.segment :: acc)
            }       
          }  
          else if ((headTag.Level > prevPureSegment.Level && !prevPureSegment.isEmpty) )
          {
          //  println("add 9 :" + index.toString + " / " + segment.segment)
             this.addToLog("add 9 :" + index.toString + " / " + headTag.Level.toString + ":" + prevPureSegment.Level.toString)
              this.addSegment(nextClause, index, segment.segment)
             segment.segment.setClause(nextClause)
             estimateClauseNum(segments.tail,headTag.tag, nextClause, segment.segment :: acc)
          }
          else 
          {
              this.addToLog("add 8:" + index.toString )
              
	          this.addSegment(actualClause, index, segment.segment)
              segment.segment.setClause(actualClause)
	          estimateClauseNum(segments.tail,headTag.tag,  actualClause, segment.segment :: acc)
          }
       }
      }
    }
    
    if (segments.filter(t => t.analyzed.HaveActiveVerb).length > 0)
    {
    estimateClauseNum(segments.zipWithIndex.toList,"", 1, List[Segment]())
    }
    else {
      getSimpleClause(segments,List[Segment]())
    }
 }
 
 private def getSimpleClause(segments : List[TaggedSegment], acc : List[Segment]) : List[Segment] = {
   if (segments.isEmpty) acc.reverse
   else if (segments.tail.isEmpty) {
     val endSegment : TaggedSegment = segments.head
      val nextSegment = endSegment.segment;
     if (endSegment.analyzed.IsBoundarySegment)
     {       
       nextSegment.setClause(0)     
     }
     else {
       nextSegment.setClause(1)
     }
      (nextSegment :: acc).reverse
   }
   else {
     val nextSegment  = segments.head.segment
     nextSegment.setClause(1)
     getSimpleClause(segments.tail, nextSegment :: acc)
   }
 }
private def updateClause(actualClause : Int, acc: List[Segment]) : Boolean = {
  if (acc.isEmpty) false
  else {
     val infoSeg = new InfoSegment(acc.head)
     val change : Boolean = {
        if (infoSeg.IsBoundarySegment) {
          updateClause(actualClause,acc.tail) 
        } else {
          actualClause == acc.head.clause
        }
     }
        
      if (infoSeg.IsBoundarySegment && change) {
        acc.head.setClause(actualClause)
      } 
      change
     }
} 
 
private def getPreviousSegment(segments : List[Segment]) : Segment = {
   
    if (segments.isEmpty) null // nema nastat
    else {
      val data = new InfoSegment(segments.head)
      if (data.IsBoundarySegment) getPreviousSegment(segments.tail)
      else segments.head
    }
   
 }
 
private def getPreviousSegmentTag(segments : List[Segment]) : Tag = {
   
    if (segments.isEmpty || !containsPureSegment(segments)) new Tag("")
    else {
      val data = getPreviousSegment(segments)
      new TaggedSegment(data).GetTag
    }
   
 }

private def containsPureSegment(data : List[Segment]) : Boolean = {
  if (data.isEmpty) false
  else if ((new InfoSegment(data.head).IsBoundarySegment))
  {
    containsPureSegment(data.tail)
  }
  else {
    true
  }
}

  private def prevClauseHaveVerb(data : List[Segment]) : Boolean = 
  {
    if (data.isEmpty || !containsPureSegment(data)) false
    else 
    {
        val segment = getPreviousSegment(data)
        val clause : List[Segment] = getWholeClause(segment.clause, data)
        clause.filter(p => new InfoSegment(p).HaveActiveVerb).length > 0
    }
  }
  
  private def superiorClauseHaveVerb(tag : Tag, data :  List[Segment]) : Boolean = 
  {
    if (data.isEmpty || !containsPureSegment(data)) false
    else 
    {
       val clauseNum : Int = getPreviousSegmentWithLevel(tag.Level,data)
       val clause : List[Segment] = getWholeClause(clauseNum, data)
       clause.filter(p => new InfoSegment(p).HaveActiveVerb).length > 0
    }
  }
  private def getWholeClause(clauseNum : Int, segments : List[Segment]) : List[Segment] =
  {
    segments.filter(p => p.clause == clauseNum).toList
  }
  
  private def getPreviousSegmentWithLevel(level : Int , data : List[Segment]) : Int =
  {
    val segments = data.filter(p => (p.level.getExactLevel == level && (!new InfoSegment(p).IsBoundarySegment)))
    if (!segments.isEmpty) {
     val segment = segments.head
     segment.clause
    }
    else {
     data.head.clause
    } 
  }
}

 
