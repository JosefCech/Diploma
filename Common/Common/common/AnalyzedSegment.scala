package common

import wordProperties._

class AnalyzedSegment(val segment : Segment) {

  def haveSubFlag = segment.words.map(f => f match {
    case f : MorfWord => f
    case f : Word => new MorfWord(f.form,"")
  }).filter(f => f.isSubFlag).size > 0
  
  protected def morfWords = segment.words.map(f => f match {
    case f : MorfWord => f
    case f : Word => new MorfWord(f.form,"")
  })
  
  def boundary = morfWords.takeWhile(p => p.isSeparator)
  def rest = morfWords.reverse.takeWhile(p => !p.isSeparator)
  
  def haveActiveVerb = TagQuery.activeVerb(this.morfWords).size > 0
  
  def haveCordConjuction =  TagQuery.cordConjuction(this.boundary).size > 0
  
  def haveReflexivePronoun = TagQuery.reflexivePronoun(this.morfWords).size > 0
  
  def haveOpeningBracket = WordFormQuery.openBracket(this.boundary) 
  
  def haveCloseBracket = WordFormQuery.closeBracket(this.boundary)
  
  def haveQuotationMark = WordFormQuery.quotationMark(this.boundary)
  
  def countWords = segment.words.size
  
  def isBoundarySegment = segment match {
    case s : Boundary => true
    case _  => false
  }
}