package common.sentence


import common.Word

class Sentence(val Words : List[Word], val ident : String ) {

 def this(Words : List[Word] ) = this(Words, "")
  
  override def equals(other : Any) : Boolean = other match {
    
    case other : Sentence =>  this.compareWords(other.Words)
    case other : MorfSentence => this.compareWords(other.morfWords)
    case _  => false
  } 
  
  def compareWords(otherWords : List[Word]) : Boolean =
  {
    def compareWords(otherWords : List[Word], myWords : List[Word]) : Boolean =
    {
      if (otherWords.isEmpty && myWords.isEmpty) true
      else if (otherWords.isEmpty || myWords.isEmpty) false        
      else if (otherWords.head != myWords.head) false
      else compareWords(otherWords.tail,myWords.tail)
    }
  compareWords(otherWords,this.Words)
  }
  
  override def toString = Words.map(s => s match {
    case s : Word => s.form
  	}
   ).mkString(" ") 
}