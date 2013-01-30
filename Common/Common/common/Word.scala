package common

import java.io;

class Word(val form : String ) {

  override def equals(that : Any) : Boolean = that match {
    case that : Word => that.form.toLowerCase == this.form.toLowerCase
    case that : String => this.form == that
   
  }
  
  override def hashCode = form.hashCode
  
  def syntacticOpposite : String = form match {
    case "["  => "]"
    case "("  => ")"
    case "{"  => "}"
    case "\"" => "\""
    case "'"  => "'"
    case "-"  => "-"
    case _ => ""
  }
}

class MorfWord( form : String, val tag: String) extends Word(form) 
{
   override def equals(that : Any) : Boolean = that match {
    case that : MorfWord => (that.form.toLowerCase == this.form.toLowerCase && that.tag == this.tag)
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
  
  def isSubFlag = (this.compareTag("J,") || this.compareTag("P4") || this.compareTag("PE") || this.compareTag("PJ") || this.compareTag("PK") || this.compareTag("PQ") || this.compareTag("PY") || this.compareTag("C?") || this.compareTag("Cu") || this.compareTag("Cz"))

}