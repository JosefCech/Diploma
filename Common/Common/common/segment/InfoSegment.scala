package common.segment

import common.{MorfWord,Word}
import wordProperties.{ WordFormQuery, TagQuery }
import wordProperties.TagMatcher

class InfoSegment(val segment : Segment){
  
  protected val subflags = segment.words.map(f => this.CreateMorfWord(f)
  ).filter(f => f.isSubFlag).toList
  def HaveSubFlag = subflags.size > 0 || segment.words.head.form == "kdy" || segment.words.head.form == "kde" 
  def FirstSubflag = { if (subflags.isEmpty) this.CreateMorfWord(segment.words.head)
		  			   else subflags.head 
                     }
  
  def morfWords = segment.words.map(f => f match {
    case f : MorfWord => f
    case f : Word => new MorfWord(f.form,"")
  })
  
  protected val verbs = TagQuery.activeVerb(this.morfWords)
  def ActiveVerb = {
                   if (verbs.isEmpty) new MorfWord("","")
  				   else verbs.head
  				   }
  def HaveActiveVerb = verbs.size > 0
  
  protected val cordConjuctions = TagQuery.cordConjuction(this.morfWords)
  def HaveCordConjuction =  cordConjuctions.size > 0
  def CordCordConjuction = cordConjuctions.head
  
  protected val reflexivePronouns = TagQuery.reflexivePronoun(this.morfWords)
  def ReflexivePronoun = reflexivePronouns.head
  def HaveReflexivePronoun = reflexivePronouns.size > 0
  

  def HaveOpeningBracket = WordFormQuery.openBracket(this.morfWords) 

  def HaveCloseBracket = WordFormQuery.closeBracket(this.morfWords)
  
  def HaveQuotationMark = WordFormQuery.quotationMark(this.morfWords)
  
  def HaveDash : Boolean = (segment.words.count(p => p.form == "-") > 0)
  
  def HaveComma : Boolean = (segment.words.count(p => p.form == ",") > 0)
  
  def CountWords = segment.words.size
  
  def IsBoundarySegment = segment match {
    case s : Boundary => true
    case _  => false
  }
  
  private def CreateMorfWord(f : Word) : MorfWord = f match
  {
   case f : MorfWord => f
   case f : Word => new MorfWord(f.form,"")
  }
}