package common

class Lemma(val lemma : String){
  
  def LemmaForm = {
    if (lemma.contains("_")) {
     val index = lemma.indexOf("_")
     lemma.substring(0, index);
    }
    else {
      lemma
    }
  }
  
  def LemmaFullForm = lemma
  
  override def equals(that : Any ) : Boolean = that match {
    case that : String => 	(this.LemmaForm.toLowerCase == that.toLowerCase) || (that.contains("_") && that == this.LemmaFullForm)
    case that : Lemma => 	 that.LemmaFullForm == this.LemmaFullForm || (this.lemma.isEmpty || that.lemma.isEmpty)
    case  _	  => false	
  }
  
  override def hashCode = lemma.hashCode
  override def toString() : String = this.LemmaForm
}