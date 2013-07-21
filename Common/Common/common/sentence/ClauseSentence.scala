package common.sentence

import common.segment.{ InfoSegment , TaggedSegment, Segment }
import common.Tag
import common.segment.AnalyzedSegment
import common.clause.Clause
import common.clause.Clause
import common.Word
import common.MorfWord
import common.segment.BaseSegment
import log.SimpleLog

trait ClauseSentence extends SimpleLog {

 private var clauses : List[(Int,Clause)] = List[(Int,Clause)]() ;
 private var lastClause : Int = 0
 var inBracket : Boolean = false
 var containsVerb : Boolean = false 
 var countDash : Int = 0


 private def maxClauseNum =
   if (clauses.isEmpty) {
     -1
   }
   else{
     clauses.map(f => f._1).max
   }
 
 private def tryAddSegment(c :  Int, s : Segment) : Boolean = {
   if (clauses.isEmpty || this.maxClauseNum < c || c == 0) {
     false
   }
   else {
     val clausesSelected : List[(Int,Clause)] = clauses.filter(p => p._1 == c).toList
     if (!clausesSelected.isEmpty)
     {
      val clause =  clausesSelected.head._2
      clause.tryAddSegment(s)
     }
     else{
       false
     }
   }
 }
 
 private def addSegment(c : Int , i : Int, s : Segment ) : Unit = 
 {
    def getPreviousClause(actual : Int) : Int =
	 {
	   val data = this.clauses.filter(p => p._2.level <= actual && p._2.isOpen).toList.sortBy(f => f._1).toList.reverse
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
   if (data.isEmpty){
     getPreviousClause(actual)
   }
   else{
     data.head._1
   } 
 }
   if (this.clauses.filter( f => f._1 == c).length > 0)
   {
	   clauses.filter(p => p._1 == c).head._2.addSegment(i, s)
   }
   else {
     val actualLevel = s.level.getExactLevel
     val previous = getPreviousClause(actualLevel)
     val superior = getSuperiorClause(actualLevel)
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
   if (new InfoSegment(acc.head).IsBoundarySegment && !new InfoSegment(acc.head).HaveCordConjuction) {
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
        if (!inBracket && segments.head.HaveOpeningBracket) {
         
           countEstimate(segments.tail,true,countBoundary,haveVerb,acc + 1)
        }
        else if ((!inBracket) && segments.head.HaveCloseBracket) {
         
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
   
 def estimateClause(segments : List[TaggedSegment]) : List[Segment] = 
 {
   countDash = segments.map(t => t.analyzed.morfWords).flatten.filter(p => p.form == "-" && p.compareTag("Z:")).length
   val firstEstimate =  estimateClauseNum(segments)
   val firstClauses = firstEstimate.zipWithIndex.groupBy(p => p._1.clause).filter(p => p._1 != 0).toList
   val clauseWithoutVerb = firstClauses.filter(p => p._2.filter(s => new TaggedSegment(s._1).GetTag.haveActiveVerb).length == 0)
   val haveActiveVerb  =  firstEstimate.filter(s => new TaggedSegment(s).GetTag.haveActiveVerb).length > 0

   if (!clauseWithoutVerb.isEmpty && haveActiveVerb)
   {
    addToLog(segments.map(p => p.segment.clause).mkString(" / ") + "\n")
    correctEstimation(clauseWithoutVerb,firstEstimate)
   }
   else {
   firstEstimate
   }
 }
 
 def correctEstimation(clauses : List[(Int,List[(Segment,Int)])], segments : List[Segment]) : List[Segment] = {
   if (clauses.isEmpty){
     segments
   }
   else{
     // update
     val emptyClause : Int = clauses.head._1
     val levelClause : Int = clauses.head._2.head._1.level.getExactLevel
     val indexStartClause : Int = clauses.head._2.head._2
       val indexedSegments : List[(Segment,Int)] = segments.zipWithIndex.filterNot(p => new TaggedSegment(p._1).analyzed.IsBoundarySegment )
      
        
     val nextLevelSegments = indexedSegments.filter(p => p._1.level == levelClause && p._1.clause > emptyClause).toList
     val prevLevelSegments = indexedSegments.filter(p => p._1.level == levelClause && p._1.clause < emptyClause).toList
     val nextSegments = indexedSegments.filter(p =>  p._1.clause > emptyClause).toList
     val prevSegments = indexedSegments.filter(p =>  p._1.clause < emptyClause).toList
     
     if (!nextLevelSegments.isEmpty){
      var changeClause = nextLevelSegments.head._1.clause
       var indexEndChange = nextLevelSegments.filter(p => p._1.clause == changeClause).sortBy(f => f._2).reverse.head._2
        this.addToLog("nextLevelSegments")
        this.addToLog("indexClause / levelClause / indexStartClause / changeClause / indexEndChange" + emptyClause.toString + 
            " /"+levelClause.toString+" / "+ indexStartClause.toString +"/"+ changeClause.toString+ "/" + indexEndChange.toString + "\n")
  
       var i = 0
       segments.foreach(f => {
         val noExistsOtherClauses : Boolean = 
            indexedSegments.filter(p => {
                          p._2 > i-2 && p._2 < indexEndChange && p._1.clause != changeClause && p._1.clause != emptyClause
            }).isEmpty
         if (i > indexStartClause && ( (f.clause == 0 && noExistsOtherClauses ) || f.clause == changeClause) &&  i <= indexEndChange  ) {
           f.setClause(emptyClause)
           addToLog("change" + i.toString + "/ " + emptyClause.toString)
         }
         else if (f.clause > changeClause)
         {
            if (f.clause != 0 )
             {
               val newIndex = f.clause - 1
               f.setClause(newIndex)
             }  
         }
          addToLog("add" + i.toString + "| " + f.clause.toString + " | " + indexStartClause.toString + " | " + indexEndChange.toString  + " | "+ changeClause.toString)
         i+=1
       })
     }
     else if(!prevLevelSegments.isEmpty)  {
       var changeClause : Int = prevLevelSegments.sortBy(f => f._1.clause).reverse.head._1.clause
        var indexStartChange : Int = prevLevelSegments.sortBy(f => f._1.clause).reverse.head._2
        var indexEndChange : Int = clauses.head._2.sortBy(p => p._2).reverse.head._2
         this.addToLog("prevLevelSegments")
         this.addToLog("indexClause / levelClause / indexStartClause / changeClause / indexEndChange" + emptyClause.toString + 
            " /"+levelClause.toString+" / "+ indexStartClause.toString +"/"+ changeClause.toString+ "/" + indexEndChange.toString + "\n")
       var i = 0
       segments.foreach(f => {
         val noExistsOtherClauses : Boolean = 
            indexedSegments.filter(p => {
                          p._2 > i-2 && p._2 < indexEndChange && p._1.clause != changeClause && p._1.clause != emptyClause
            }).isEmpty
         if (i > indexStartChange && ((f.clause == 0 && noExistsOtherClauses ) || f.clause == emptyClause) &&  i <= indexEndChange  ) {
           f.setClause(changeClause)
           addToLog(f.clause.toString + "\n")
         }
         else if (indexEndChange < i)
         {
             if (f.clause != 0 )
             {
               val newIndex = f.clause - 1
               f.setClause(newIndex)
             }  
           
         }
         i+=1
       })
     }
     /*
     else if (!nextSegments.isEmpty) {
        def changeClause = nextSegments.head._1.clause
       def indexEndChange = nextSegments.filter(p => p._1.clause == changeClause).sortBy(f => f._2).reverse.head._2
        this.addToLog("indexClause / levelClause / indexStartClause / changeClause / indexEndChange" + emptyClause.toString + 
            " /"+levelClause.toString+" / "+ indexStartClause.toString +"/"+ changeClause.toString+ "/" + indexEndChange.toString)
  
       var i = 0
       segments.foreach(f => {
         if (i > indexStartClause && (f.clause == 0 || f.clause == changeClause) &&  i <= indexEndChange  ) {
           f.setClause(emptyClause)
         }
         i+=1
       })
     }
     else if (!prevSegments.isEmpty) {
        def changeClause : Int = prevSegments.sortBy(f => f._1.clause).reverse.head._1.clause
        def indexStartChange : Int = prevSegments.sortBy(f => f._1.clause).reverse.head._2
        def indexEndChange : Int = clauses.head._2.sortBy(p => p._2).reverse.head._2
       var i = 0
       segments.foreach(f => {
         if (i > indexStartChange && (f.clause == 0 || f.clause == emptyClause) &&  i <= indexEndChange  ) {
           f.setClause(changeClause)
         }
         i+=1
       })
     }*/
     correctEstimation(clauses.tail,segments)
   }
   }
 
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
        val prevTag : Tag = new Tag(previousTag)
        val prevPureSegment : Tag = getPreviousSegmentTag(acc);
        val nextClause = 
        {
        		this.getNextClause
        }
        
        if (segment.analyzed.IsBoundarySegment && (segment.analyzed.HaveOpeningBracket) && acc.isEmpty)
        {
          this.addToLog( "add 12 :" + index.toString )
          this.inBracket = true
          segment.segment.setClause(0)
          this.addSegment(0, index, segment.segment)
	      estimateClauseNum(segments.tail,headTag.tag, actualClause, segment.segment :: acc)
        }
        else if (
        			(segment.analyzed.IsBoundarySegment && 
        		    (segment.analyzed.HaveOpeningBracket || segment.analyzed.HaveCloseBracket || (segment.analyzed.HaveDash && this.countDash == 2) ) ) ||
        		    ((this.inBracket ) ))
        {
          this.addToLog( "add 10 :" + index.toString + this.inBracket.toString + " " +segment.analyzed.HaveDash.toString)
          
          if (segment.analyzed.HaveOpeningBracket || (segment.analyzed.HaveDash && !this.inBracket)) 
          {
            this.inBracket = true
          }
          else if (segment.analyzed.HaveCloseBracket || (segment.analyzed.HaveDash && this.inBracket)) 
          {
            this.inBracket = false
          }
          
          if (segment.analyzed.HaveCloseBracket && this.containsVerb)
          {
            this.updateInBracket(acc,nextClause,index-1,false)
            this.containsVerb = false
          }
          else if (this.inBracket && headTag.haveActiveVerb) 
          {
            this.containsVerb = true
          }
          
          if (!segments.tail.isEmpty && (segments.tail.head._1.analyzed.HavePunct || segments.tail.head._1.analyzed.HaveComma) && (!segments.tail.head._1.analyzed.HaveDash))
          {
            segment.segment.setClause(0)
            this.addSegment(0, index, segment.segment)
	        estimateClauseNum(segments.tail,headTag.tag, actualClause, segment.segment :: acc)
          }
          else 
          {
            segment.segment.setClause(actualClause)
	        this.addSegment(actualClause, index, segment.segment)
	        estimateClauseNum(segments.tail,headTag.tag, actualClause, segment.segment :: acc)
          }
        }
        else if (headTag.isBoundary && headTag.haveCordConj && (prevTag.haveComma || prevTag.interpunction) && !segments.tail.isEmpty && segments.tail.head._1.analyzed.HaveSubFlag)
        {
          this.addToLog( "add 11 :" + index.toString )
          segment.segment.setClause(nextClause)
          this.addSegment(nextClause, index, segment.segment)
          estimateClauseNum(segments.tail,headTag.tag,nextClause, segment.segment :: acc)
        }
        else if (headTag.isBoundary && !acc.isEmpty)
        {
          this.addToLog( "add 1 :" + index.toString )
          this.addToLog(prevTag.tag + "\n")
          segment.segment.setClause(0)
          this.addSegment(0, index, segment.segment)
          estimateClauseNum(segments.tail,headTag.tag, actualClause, segment.segment :: acc)
        }
        else if (headTag.isBoundary &&  headTag.compare(5, "Z") &&  acc.isEmpty )
        {
          addToLog("add 2:" + index.toString )
          segment.segment.setClause(0)
          this.addSegment(0, index, segment.segment)         
          estimateClauseNum(segments.tail,headTag.tag, actualClause, segment.segment :: acc)
        }        
        else 
       {
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
            else 
            {
               this.addToLog("add 5:" + index.toString)
               this.closeClause(actualClause)
               this.setNullBoundaryClause(acc)     
               segment.segment.setClause(nextClause)
              this.addSegment(nextClause, index, segment.segment)
             estimateClauseNum(segments.tail,headTag.tag, nextClause, segment.segment :: acc)
            }
          }                
          else if ((headTag.Level < prevPureSegment.Level && !prevPureSegment.isEmpty) && !prevTag.haveDash)
          {
            val superiorClause = this.getSuperiorClause(prevPureSegment.Level)
            val addToClauseSegment = this.tryAddSegment(superiorClause, segment.segment)
            this.addToLog("add 6 :" + addToClauseSegment)
            this.addToLog("add 6 :" + superiorClause)
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
          else if ((headTag.Level > prevPureSegment.Level && !prevPureSegment.isEmpty) && !prevTag.haveDash )
          {
             this.addToLog("add 9 :" + index.toString + " / " + headTag.Level.toString + ":" + prevPureSegment.Level.toString)
             this.addSegment(nextClause, index, segment.segment)
             segment.segment.setClause(nextClause)
             estimateClauseNum(segments.tail,headTag.tag, nextClause, segment.segment :: acc)
          }
          else 
          {
              this.addToLog("add 8:" + index.toString )
              this.addToLog("add 8:" + headTag.isBoundary )
              this.addToLog(segment.analyzed.IsBoundarySegment + " " + (segment.analyzed.CountWords > 0))
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
    else 
    {
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

 def updateInBracket(data : List[Segment], actualClause : Int, index : Int, haveAlreadyVerb : Boolean) : Int =
{
 if (data.isEmpty || BaseSegment.createInfoSegment(data.head).HaveOpeningBracket) 
   { 
     if (!data.isEmpty) { 
       data.head.setClause(0)
          addToLog("end update bracked" + data.head.toString )
     }
     else {
       addToLog("end sentence \n" )
     }
     addToLog("end update bracked" )
     actualClause
   }
 else {
   var clause = actualClause
   if (haveAlreadyVerb  && BaseSegment.createInfoSegment(data.head).HaveActiveVerb)
   {
     clause += 1
   }
   data.head.setClause(clause)
   this.addSegment(clause, index, data.head)
   updateInBracket(data.tail, clause,index-1,BaseSegment.createInfoSegment(data.head).HaveActiveVerb)
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

 
