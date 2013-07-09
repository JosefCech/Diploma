package common

import java.io;


trait AbstractWord {
}

class Word(val form : String ) extends AbstractWord {

  override def equals(that : Any) : Boolean = that match {
    case that : Word => that.form.toLowerCase == this.form.toLowerCase
    case that : String => this.form == that
   
  }
  
  override def hashCode = form.hashCode
 
  
  def syntacticOpposite : String = form match {
    case "["  => "]"
    case "("  => ")"
    case "{"  => "}"
   // case "\"" => "\""
  //  case "'"  => "'"
   // case "-"  => "-"
    case _ => ""
  }
  
  def isEmpty = form == ""
}

object Word {
  def createEmptyWord = new Word("")
  def createMorfWord(s : Word) : MorfWord = s match {
    case s : MorfWord => s
    case s : Word => new MorfWord(s.form,"")
  }
}
class RuleWord( form : String, tag : String) extends Word(form)
{
 val tags = tag.split(";") 
 val emptyTag = "_______________"
   
 def createFullTag(tag : String) : String = tag + emptyTag.take(emptyTag.length-tag.length) 
 
 override def equals(that : Any) : Boolean = that match {
   case that : MorfWord => {
                              val tagMatch = !this.tags.filter( p => wordProperties.TagMatcher.Match(that, this.createFullTag(p))).isEmpty
                              if (this.form.isEmpty) tagMatch
                              else form == that.form
                             }
   case _ => super.equals(that)
 }
}

class MorfWord( form : String, val lemma: Lemma , val tag: String , val ident: String) extends Word(form) 
{
  def this(form: String, lemma: Lemma, tag: String) = this(form, lemma, tag, "")
  def this(form: String, lemma : String, tag : String) = this(form,new Lemma(lemma),tag)
  def this(form : String , tag: String) =  this(form,"",tag)
  def this(form: String, lemma : String, tag : String, ident : String) = this(form,new Lemma(lemma),tag, ident)
  
   override def equals(that : Any) : Boolean = that match {
    case that : MorfWord => (that.form.toLowerCase == this.form.toLowerCase && that.tag == this.tag && this.lemma == that.lemma)
    case that : Word => this.form == that.form
    case that : String => this.form == that
  }
  
  override def hashCode = form.hashCode + tag.hashCode
  
  def compareTag( tagCompare : String) : Boolean = {
    def partialCompare(tag1 : String , tag2: String ) : Boolean = {
      if (tag1.isEmpty || tag2.isEmpty) true
      else {
         val ch1 = tag1.head
         val ch2 = tag2.head
         if (ch1 == ch2) partialCompare(tag1.tail,tag2.tail)
         else false
      }
    }
    partialCompare(tag,tagCompare)
  }
  
  def isSeparator = ((this.compareTag("J^") || this.compareTag("Z:")) && (!("%/+*><&".contains(form.trim))))
  
  def isSubFlag = (this.compareTag("J,") || this.compareTag("P9") || this.compareTag("P4") || this.compareTag("PE") || this.compareTag("PJ") || this.compareTag("PK") || this.compareTag("PQ") || this.compareTag("PY") || this.compareTag("C?") || this.compareTag("Cu") || this.compareTag("Cz") || this.compareTag("P1")) && this.form != "jako" 

}

class AWord(val ident : String, val ord : Int, val clauseNum : Int){
  override def toString = "ident : " + ident + " clause:" + clauseNum.toString + " ord:" + ord.toString
 }

class AnalyzedWord (val word : MorfWord, val clauseNum : Int, val separator : Boolean) extends AbstractWord{
 }

class ClauseInfo(val Ident : String, val ClauseNum : Int){
   override def toString = "ident : " + Ident + " clause:" + ClauseNum.toString 
}

class SegmentInfo(val index : Int, val level: Int, val isJoined : Boolean) {
  override def toString = "index : " + index.toString + " level:" + level.toString + " join: " + isJoined
}