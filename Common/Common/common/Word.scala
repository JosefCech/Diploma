package common

import java.io;

class Word(val form : String ) {
  def IsSeparator : Boolean = false
  override def equals(that : Any) : Boolean = that match {
    case that : Word => that.form.toLowerCase == this.form.toLowerCase
    case that : String => this.form == that
   
  }
  
  override def hashCode = form.hashCode
}

class MorfWord( form : String, val tag: String) extends Word(form) 
{
   override def equals(that : Any) : Boolean = that match {
    case that : MorfWord => (that.form.toLowerCase == this.form.toLowerCase && that.tag == this.tag)
    case that : Word => this.form == that.form
    case that : String => this.form == that
  }
  
  override def hashCode = form.hashCode + tag.hashCode
}