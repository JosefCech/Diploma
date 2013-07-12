package common.sentence

import common.segment.{ Segment, Boundary, PureSegment, InfoSegment, TaggedSegment,BaseSegment }
import common.{ Word }
import wordProperties.TagMatcher
import log.SimpleLog
import common.MorfWord

trait LevelEstimateSentence extends SimpleLog{
  
  var coordConj : Boolean = false
    
  /**
   * Vrátí segmenty s úrovní zanoøení
   */
  def estimateLevelSegments(segments : List[Segment]) : List[Segment] = {
   
    // if subflag level + 1, previous segment start new clause
    // if boundary == "," level -1, previous segment start newclause , if next has  subflag
       // next try => level max-up
    // if two segments with verb => second start newclause
    estimateLevels(segments,0,List[String](),List[Segment]())
  }
  
  /**
   * Vrátí segmenty s úrovní zanoøení - rozšíøená verze s acc
   */
  private def estimateLevels(segments : List[Segment], actualLevel : Int, dualBorder : List[String], acc : List[Segment]) : List[Segment] = {
   
    if (segments.isEmpty) 
    {
      // konec vrátím otoèený zásobník
      acc.reverse
    } 
    else 
    {
      
     if (segments.tail.isEmpty && segments.head.isBoundary) 
     {   // pokud je pøedposlední segment je hranice napø. ) nastavit úroveò na 0
         this.appendLog("add 0 : " + segments.head.toString )
	     var boundary =  setLevelSegment(segments.head,0, false )
	     estimateLevels(segments.tail, actualLevel, dualBorder, boundary :: acc)
     } 
     else if (segments.head.isBoundary ) 
     {   // pokud je aktuální objekt hranice - pøipravím si ji na aktuální úroveò
         this.appendLog("add 1 : " + segments.head.toString )
	     var boundary =  setLevelSegment(segments.head,actualLevel, false )
	     //  má nìjaký protiklad napø. závorky
	     val opposite =  boundary.words.head.syntacticOpposite
	     var level = actualLevel
	     val info = new InfoSegment(boundary)
	     val prevHaveVerb = {
	        // pokud pøedchozí segment má sloveso 
		     if (acc.isEmpty) 
		     {
		       false
		     }
		     else 
		     {
		       new InfoSegment(acc.head).HaveActiveVerb
		     }
         }
	 // aktuální objekt má pøíznak podøízenosti     
     val prevHaveSubflag = {
	     if (acc.isEmpty) 
	     {
	       false
	     }
	     else 
	     {
	       new InfoSegment(acc.head).HaveSubFlag
	     }
     }
     // pøípadné pøidání nebo upravení úrovnì zanoøení
     val newDualBorder : List[String] = {
    		 if (isRequiredDualBorder(boundary,dualBorder))
             {
              level = this.minusLevel(level)
              boundary.setLevel(level)
              dualBorder.tail
             }
             else if (opposite.isEmpty) 
             {
              dualBorder
             }
             else 
             {
              level += 1
              opposite :: dualBorder
             }
     }
     // konec získávání informací 
      // zaèátek rozhodování
     if (nextSegmentSubflag(segments.tail))
	 { // následující segment má subflag
      val prev = {
        if (acc.isEmpty) 
        {
         BaseSegment.createEmptySegment.createInfoSegment
        }
        else 
        {
         acc.head.createInfoSegment
        }
      }
      val next = segments.tail.head.createInfoSegment
       var checkSubflag = this.checkSubFlagsSegment(prev,next)
      if (info.HaveComma && !acc.isEmpty && checkSubflag)
      { // segA , segB => obsahuji podobné pøíznaky podøízenosti oddìlené èárkou 
       level += 1
       this.coordConj = false;
       boundary.setLevel(0)    
       this.appendLog("add 8 : " + level.toString +"/"+segments.tail.head.toString )
      }
      else if (info.HaveComma && !acc.isEmpty && !checkSubflag && !next.HaveFakeSubflag) 
      {
         this.coordConj = false;
          boundary.setLevel(0)    
          this.appendLog("add 9 : " + level.toString +"/"+segments.tail.head.toString )
      }
      this.appendLog("add 2 : " + level.toString +"/"+segments.head.toString )
	  val nextSegment = setLevelSegment(segments.tail.head,level, true )
	  estimateLevels(segments.tail.tail,level, newDualBorder , nextSegment :: boundary :: acc)
	 }
     else if ((info.HaveComma) && nextSegmentCordConjuctionWithNoComma(segments.tail))
     { // , a|i => sniž úroveò o jedna
       val nextLevel = {
         if (!segments.tail.isEmpty && nextSegmentSubflag(segments.tail.tail))
         {
           level+1
         }
         else 
         {
           this.minusLevel(level)
         }
       }
       this.coordConj = true;
       boundary.setLevel(0)  
       this.appendLog("add 3 : " + level.toString +"/"+segments.head.toString )
       estimateLevels(segments.tail, nextLevel , newDualBorder, boundary :: acc)
      }
      else if (prevHaveSubflag && prevHaveVerb && nextSegmentActiveVerb(segments.tail) && info.HaveComma)
      { // segA , segB => segA obsahuje pøíznak podøízenosti a sloveso a následující segment obsahuje sloveso
        val nextLevel = this.minusLevel(level)
        this.coordConj = false;
        boundary.setLevel(0)         
         this.appendLog("add 4 : " + level.toString +"/"+segments.head.toString )
        estimateLevels(segments.tail, nextLevel , newDualBorder, boundary :: acc)    
      }
      else if (info.HaveComma && this.coordConj)
      { // , a v pøedchozím byly segmenty spojeny spojkou
        val nextLevel = this.minusLevel(level)
        this.coordConj = false;
        boundary.setLevel(0)      
         this.appendLog("add 5 : " + level.toString +"/"+segments.head.toString )
        estimateLevels(segments.tail, nextLevel , newDualBorder, boundary :: acc)
      }
    
	  else
	  { // v ostatních pøípadech použíj aktuální úroveò zanoøení
	     this.appendLog("add 6 : " + level.toString +"/"+segments.head.toString )
	     estimateLevels(segments.tail, level, newDualBorder, boundary :: acc)
	  }
    }
    else 
    { // objekt není boundary => segment
      var level = actualLevel
      if (nextSegmentSubflag(segments))
      { // obsahuje subflag add level
         level+=1;
      }
      if (!acc.isEmpty)
      { // nejsem na zacatku
         val head = new InfoSegment(acc.head)
         if (head.IsBoundarySegment && head.segment.level != level &&  nextSegmentActiveVerb(segments))
         { // predchozi objekt byl hranice a mam ruzne levely nastav a obsahuju sloveso  
        	 if (head.HaveCordConjuction)
        	 {
        		 this.coordConj = true;
        	 }
        	 acc.head.setLevel(0)
         }
       }
        this.appendLog("add 7 : " + level.toString +"/"+segments.head.toString )
       val nextSegment = setLevelSegment(segments.head,level,false)
       estimateLevels(segments.tail, actualLevel, dualBorder, nextSegment :: acc)
    }
   }
  }

  /**
   * následující segment obsahuje pøíznak podøízenosti
   */
  private def nextSegmentSubflag(l : List[Segment]) : Boolean = 
  {
   if (l.isEmpty ) false
   else {
	   containSubflag(l.head)
   }
  }
  
  /**
   * Segment obsahuje pøíznak podøízenosti
   */
  private def containSubflag(s : Segment) : Boolean = {
   val data = new InfoSegment(s)
   data.HaveSubFlag
  }

  /**
   * Nastavení úrovnì zanoøení a poèátek nové clause
   */
  private def setLevelSegment(s : Segment, level : Int , startNewClause : Boolean) : Segment = {
    s.setLevel(level)
    if (startNewClause) s.setStartNewClause()
    s
  }
  /**
   * následující segment je hranice
   */
  private def nextSegmentBoundary(l : List[Segment]) : Boolean = 
  { 
     if (l.isEmpty ) false
     else {
	 l.head.isInstanceOf[Boundary];
     }
  }
  /**
   * další segment obsahuje spojky, pøed kterými nesmí být èárka
   */
  private def nextSegmentCordConjuctionWithNoComma(l : List[Segment]) : Boolean ={
    if (nextSegmentBoundary(l) &&  (new InfoSegment(l.head).HaveCordConjuction))
    {
       val coords = List("a","i")
       l.head.words.filter(p => coords.contains(p.form)).length > 0
    }
    else {
      false
    }
  }
  /**
   * další segment obsahuje aktivní sloveso
   */
  private def nextSegmentActiveVerb(l : List[Segment]) : Boolean = {
    if (l.isEmpty) {
      false
    }
    else {
      new InfoSegment(l.head).HaveActiveVerb
    }
  }

  /**
   * kontrola zda hranice není ukonèovací  závorka 
   */
  private def isRequiredDualBorder(s : Segment, b : List[String]) : Boolean =
  {
    if (b.isEmpty) false
    else 
    {
	    val actualBorder = s.words.head.form
	    val requiredBorder = b.head
	    actualBorder == requiredBorder
    }
  }
  /**
   * kontrola zda pøedchozí a následující segment mají pøíznak podøízenosti
   */
  private def checkSubFlagsSegment(prev : InfoSegment , next : InfoSegment) : Boolean = {
    
    this.appendLog(prev.HaveSubFlag.toString +"/"+next.HaveSubFlag +"/"+ next.HaveFakeSubflag)
    if (prev.HaveSubFlag && next.HaveSubFlag && !next.HaveFakeSubflag)
    {
      if (!next.FirstSubflag.tag.startsWith("P") && !TagMatcher.Match(prev.FirstSubflag, next.FirstSubflag.tag.take(2).toString)) 
       {
	     prev.FirstSubflag.tag.startsWith("P")            
         } else 
         {
          true //TagMatcher.Match(prev.FirstSubflag, next.FirstSubflag.tag.head.toString) && prev.FirstSubflag.form != next.FirstSubflag.form
        }
    } else { 
      !next.HaveFakeSubflag
    }
  }
  /**
   * zmenšení úrovnì zanoøení nesmí klesnout pod 0
   */
  private def minusLevel(level : Int) : Int = {
     if (level == 0) {
               0
        }
        else {
            level-1
        }
  }
}