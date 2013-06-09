package common.sentence
import common.Word
import common.segment.Segment

class AnxSentence (val Segments : List[Segment]) extends Sentence(Segments.map(t => t.words).flatten) {

   override def toString = {
      
      var text = ""
      this.Segments.foreach( f => text += f.ToString + "\n")
      text
      }
}