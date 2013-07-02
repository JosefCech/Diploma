package common.sentence
import common.{ MorfWord, Word}
import common.segment.Segment

class AnxSentence (val Segments : List[Segment], val Ident : String) extends Sentence(Segments.map(t => t.words).flatten) {
 
  def this(Segments : List[Segment]) = this(Segments,"")
 
   override def toString = { 
      var text = ""
      this.Segments.foreach( f => text += f.toString + "\n")
      text += this.Ident
      text
      }
   
   def analyzedSentence = {  try {
                             new AnalyzedSentence(Ident,Segments);
                             }
   							 catch {
   							   case e : Exception => null
   							 }
   }
   def morfSentence = new MorfSentence(this.Words.map(p => p match {case c : MorfWord => c}).toList, Ident)
}