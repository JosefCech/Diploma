package common.sentence
import common.{ MorfWord, Word}
import common.segment.{Segment, PureSegment, Boundary, AnalyzedSegment}
import common.segment.BaseSegment
import common.Tag

class AnxSentence ( Segments : List[Segment], val Ident : String) extends Sentence(Segments.map(t => t.words).flatten) {
 
  val segments = Segments
  def this(Segments : List[Segment]) = this(Segments,"")
 
   override def toString = { 
      var text = ""
      this.Segments.foreach( f => f match { case f : PureSegment => text += f.toString + "\n"
      										case f : Boundary => text += f.toString + "\n"
      										case f : AnalyzedSegment => text += f.toString + "\n"
      })
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
   
   def sentenceWithData =  analyzedSentence.segments
  
   
   def sentenceWithLevel : List[Segment] = { val data = Segments.map( p => p match {
                                                 case  p : AnalyzedSegment => p.data
                                                 case  p : PureSegment => p
                                                 case  p : Boundary => p
                                              } )
                                             val levelData = analyzedSentence.segments.zipWithIndex
                                             levelData.foreach(f => {
                                               data.apply(f._2).setLevel(f._1.Level)
                                               data.apply(f._2).setClause(0)
                                             })
                                             data
                                            }
  
  def getTagsWithClause : List[(Int,Tag)] = 
  {
    this.sentenceWithData.map( segment => {
      (segment.clause, segment.taggedSegment.GetTag )  
    })
    
  }
   
  def getTagsOnly : List[Tag] = this.sentenceWithData.map(segment => segment.taggedSegment.GetTag)
}