package common

import wordProperties._

class AnalyzedSegment(val segment : Segment) {
  
  protected val subflags = segment.words.map(f => f match {
    case f : MorfWord => f
    case f : Word => new MorfWord(f.form,"")
  }).filter(f => f.isSubFlag)
  def HaveSubFlag = subflags.size > 0
  def FirstSubflag = subflags.head
  
  def morfWords = segment.words.map(f => f match {
    case f : MorfWord => f
    case f : Word => new MorfWord(f.form,"")
  })
  
  protected val verbs = TagQuery.activeVerb(this.morfWords)
  def ActiveVerb = verbs.head
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
  
  def CountWords = segment.words.size
  
  def IsBoundarySegment = segment match {
    case s : Boundary => true
    case _  => false
  }
  
  def UpdateLevel(u : Int) : Unit = segment.updateLevel(u)
  
  def SetLevel(l : Int) : Unit = segment.SetLevel(l)
}