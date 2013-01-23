package Main
import common._

class RuleClauser(val sentence : List[Segment]) {

  def analyzedSegments = sentence.map(t => new AnalyzedSegment(t)).toList 
  def estimationOfClause : Int = {

      val countActiveVerb =  analyzedSegments.filter(t=> t.countWords > 0 && t.haveActiveVerb).size
       if (countActiveVerb == 0) { 
         1 
       }
       else{
          countActiveVerb
       }
      
  }
  
  def analyzeSentence : SentenceClause = {
    
     def addSegment(newSegment : AnalyzedSegment, sentenceClause : SentenceClause) : SentenceClause = {
        if (newSegment.haveActiveVerb)
        {
          
        }
     }
    
    var sentence : SentenceClause = new SentenceClause(List[(Int,List[Clause])]())
    var level = 0
    analyzedSegments.foreach(t => {
    	if (t.haveSubFlag) level += 1  
    	sentence = addSegment(t,level,sentence)
    	level += 1	
    })
    sentence
    }
  }
  
}