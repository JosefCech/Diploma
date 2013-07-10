package common.clause

import common.segment.Segment
import common.segment.InfoSegment
import common.Word
import wordProperties.TagMatcher
import common.LingvisticAggreement
/** Clause - group of tuples (Index,Segment)
  * Base functions : 
  * 		add segment
  * 		try add
  * 		haveActiveVerb
  *         haveSuperiorClause
  *         havePreviousClause
  *         close               
  * 
  * @constructor create clause from list of tuples (Index,Segment) and flag open
  * @constructor create clause from one tuple (Index,Segment) and flag - is set to true
  * @constructor create clause from list of tuples (Index,Segment) and flag - is set to true
  * @param segments - group of segments
  * @param segment - initial segment identificator  
  * @param open - flag - which tells if clause can accepted next segment or not   
  */
class Clause( segments : List[(Int,Segment)],  openClause : Boolean ) {

  def this( segments : List[(Int,Segment)]) = this(segments,true)
  def this( segment : (Int,Segment)) = this(List(segment),true) 
  
  var open : Boolean = openClause
  var data : List[(Int,Segment)] = segments
  var level : Int = 
          if (segments.isEmpty) {
            -1
            }
           else {
             segments.map(p => p._2.level).groupBy(p => p.getExactLevel).toList.sortBy(f => f._1).toList.head._1 
           }
 
  var superior: Int = -1
  var previous : Int = -1
  
  def infoSegments : List[InfoSegment] = this.data.map(p => this.createInfoSegment(p._2)).toList
  
  def addSegment(i : Int, s : Segment) = {
    this.data = (i,s) :: this.data
  }
  
  def haveActiveVerb() : Boolean = {
     this.infoSegments.filter(p => p.HaveActiveVerb).length > 0
  }

  private def activeVerbs : List[Word] = {
    val verbSeg : List[InfoSegment]= this.infoSegments.filter(p => p.HaveActiveVerb)
  
    if (verbSeg.isEmpty) {
      List[Word]()
    }
    else {
       val word = verbSeg.head.ActiveVerb
       if (word.isEmpty)
          List[Word]()
       else {
         List(word) 
       }
    
    }
  }
  def tryAddSegment(s : Segment) : Boolean = {
    open && this.verbsQuery(s) && (this.level == s.level.getExactLevel)

  } 
  
  def close = open = false
  def isOpen : Boolean = open
  
  def setPreviousClause(index : Int) = this.previous = index
  def setSuperiorClause(index : Int) = this.superior = index
  
  def havePreviousClause : Boolean = this.previous != -1
  def haveSuperiorClause : Boolean = this.superior != -1
  
  def createInfoSegment(s : Segment) : InfoSegment = s match {
    case s : InfoSegment => s
    case s : Segment => new InfoSegment(s)
  }
  
  def verbsQuery(s : Segment) : Boolean =
  {
     
    val infoSeg : InfoSegment = this.createInfoSegment(s)
    val oneIsWithoutVerb : Boolean= ((!infoSeg.HaveActiveVerb || !this.haveActiveVerb))
 
    
    if (oneIsWithoutVerb) {
       LingvisticAggreement.verbAgreement(this.data.map(p => p._2).toList, s)
    }
    else {
     
     val myVerbs = this.activeVerbs
     val nextVerb = infoSeg.ActiveVerb
     
   
      if (myVerbs.isEmpty || nextVerb.isEmpty)
      {
        true
      }
      else {
          val forms = List("jsme","jsem","jsi","jste")
          val conditionForms = List("by","bychom","byste","bys")
          val myVerb = Word.createMorfWord(myVerbs.head)
          val numberCondition = myVerb.tag.charAt(3) == nextVerb.tag.charAt(3)
          val myPerson = myVerb.tag.charAt(7).toString
          val nextPerson = nextVerb.tag.charAt(7).toString
          val personCondition : Boolean = {
            (myPerson != "X" && nextPerson == "X") ||  (myPerson == "X" && nextPerson != "X")
          }
        
          val tenseCondition = (myVerb.tag.charAt(8).toString == "P" &&  nextVerb.tag.charAt(8).toString == "R" && forms.contains(myVerb.form)
           ) || (myVerb.tag.charAt(8).toString == "R" &&  nextVerb.tag.charAt(8).toString == "P" && forms.contains(nextVerb.form))
         
          // past participle
          val participle = (TagMatcher.Match(myVerb, "VB") && TagMatcher.Match(nextVerb, "Vp") && numberCondition && personCondition && tenseCondition )
          // past condition
           val pastCondition : Boolean =  (TagMatcher.Match(myVerb, "Vc") && forms.contains(myVerb.form)) || (TagMatcher.Match(nextVerb, "Vc") && forms.contains(nextVerb.form)) 
          
          val condition : Boolean = TagMatcher.Match(myVerb, "Vc") && TagMatcher.Match(nextVerb, "Vp") && pastCondition
           
         (participle || condition)
      }
    }
    
    
    
  }
}