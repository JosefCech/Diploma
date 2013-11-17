package common.sentence

import common.segment.{ Segment, Boundary, PureSegment, InfoSegment, TaggedSegment,BaseSegment }
import common.{ Word }
import wordProperties.TagMatcher
import log.SimpleLog
import common.MorfWord

trait LevelEstimateSentence extends SimpleLog{
  
  var coordConj : Boolean = false
     
  /**
   * Vr�t� segmenty s �rovn� zano�en�
   */
  def estimateLevelSegments(segments : List[Segment]) : List[Segment] = {
   
    // if subflag level + 1, previous segment start new clause
    // if boundary == "," level -1, previous segment start newclause , if next has  subflag
       // next try => level max-up
    // if two segments with verb => second start newclause
    estimateLevels(segments,0,List[String](),List[Segment]())
  }
  
  /**
   * Vrátí segmenty  s rovn� zano�en� - rozšířená verze s acc
   */
  private def estimateLevels(segments : List[Segment], actualLevel : Int, dualBorder : List[String], acc : List[Segment]) : List[Segment] = {
   
    if (segments.isEmpty) 
    {
      // konec vr�t�m oto�en� z�sobn�k
      acc.reverse
    } 
    else 
    {
      
     if (segments.tail.isEmpty && segments.head.isBoundary) 
     {   // pokud je p�edposledn� segment je hranice nap�. ) nastavit �rove� na 0
         this.appendLog("add 0 : " + segments.head.toString )
	     var boundary =  setLevelSegment(segments.head,0, false )
	     estimateLevels(segments.tail, actualLevel, dualBorder, boundary :: acc)
     } 
     else if (segments.head.isBoundary ) 
     {   // pokud je aktu�ln� objekt hranice - p�iprav�m si ji na aktu�ln� �rove�
         this.appendLog("add 1 : " + segments.head.toString )
	     var boundary =  setLevelSegment(segments.head,actualLevel, false )
	     //  m� n�jak� protiklad nap�. z�vorky
	     val opposite =  boundary.words.head.syntacticOpposite
	     var level = actualLevel
	     val info = new InfoSegment(boundary)
	     val prevHaveVerb = {
	        // pokud p�edchoz� segment m� sloveso 
		     if (acc.isEmpty) 
		     {
		       false
		     }
		     else 
		     {
		       new InfoSegment(acc.head).HaveActiveVerb
		     }
         }
	 // aktu�ln� objekt m� p��znak pod��zenosti     
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
     // p��padn� p�id�n� nebo upraven� �rovn� zano�en�
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
     // konec z�sk�v�n� informac� 
      // za��tek rozhodov�n�
     if (nextSegmentSubflag(segments.tail))
	 { // n�sleduj�c� segment m� subflag
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
      { // segA , segB => obsahuji podobn� p��znaky pod��zenosti odd�len� ��rkou 
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
     { // , a|i => sni� �rove� o jedna
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
      { // segA , segB => segA obsahuje p��znak pod��zenosti a sloveso a n�sleduj�c� segment obsahuje sloveso
        val nextLevel = this.minusLevel(level)
        this.coordConj = false;
        boundary.setLevel(0)         
         this.appendLog("add 4 : " + level.toString +"/"+segments.head.toString )
        estimateLevels(segments.tail, nextLevel , newDualBorder, boundary :: acc)    
      }
      else if (info.HaveComma && this.coordConj)
      { // , a v p�edchoz�m byly segmenty spojeny spojkou
        val nextLevel = this.minusLevel(level)
        this.coordConj = false;
        boundary.setLevel(0)      
         this.appendLog("add 5 : " + level.toString +"/"+segments.head.toString )
        estimateLevels(segments.tail, nextLevel , newDualBorder, boundary :: acc)
      }
    
	  else
	  { // v ostatn�ch p��padech pou��j aktu�ln� �rove� zano�en�
	     this.appendLog("add 6 : " + level.toString +"/"+segments.head.toString )
	     estimateLevels(segments.tail, level, newDualBorder, boundary :: acc)
	  }
    }
    else 
    { // objekt nen� boundary => segment
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
   * n�sleduj�c� segment obsahuje p��znak pod��zenosti
   */
  private def nextSegmentSubflag(l : List[Segment]) : Boolean = 
  {
   if (l.isEmpty ) false
   else {
	   containSubflag(l.head)
   }
  }
  
  /**
   * Segment obsahuje příznak podřízenosti
   */
  private def containSubflag(s : Segment) : Boolean = {
   val data = new InfoSegment(s)
   data.HaveSubFlag
  }

  /**
   * Nastaven� �rovn� zano�en� a po��tek nov� clause
   */
  private def setLevelSegment(s : Segment, level : Int , startNewClause : Boolean) : Segment = {
    s.setLevel(level)
    if (startNewClause) s.setStartNewClause()
    s
  }
  /**
   * n�sleduj�c� segment je hranice
   */
  private def nextSegmentBoundary(l : List[Segment]) : Boolean = 
  { 
     if (l.isEmpty ) false
     else {
	 l.head.isInstanceOf[Boundary];
     }
  }
  /**
   * dal�� segment obsahuje spojky, p�ed kter�mi nesm� b�t ��rka
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
   * dal�� segment obsahuje aktivn� sloveso
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
   * kontrola zda hranice nen� ukon�ovac�  z�vorka 
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
   * kontrola zda p�edchoz� a n�sleduj�c� segment maj� p��znak pod��zenosti
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
   * zmen�en� �rovn� zano�en� nesm� klesnout pod 0
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