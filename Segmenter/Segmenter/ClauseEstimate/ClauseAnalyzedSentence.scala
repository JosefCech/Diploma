package ClauseEstimate

import common.sentence.{ MorfSentence , ClauseSentence }
import common.segment.{Segment, TaggedSegment}
import common.{ Word }
import Rules._
import LevelEstimate.LevelAnalyzedSentence
import Rules.MatchEffect
import common.segment.BaseSegment
import DataObjects.EstimateSentence
import common.sentence.AnxSentence
import common.segment.AnalyzedSegment
import scala.xml.Node
import Xml.XmlSentence

class ClauseAnalyzedSentence(sentence : List[Word],  ident : String , val levelWithSegments : List[Segment]) 
	extends MorfSentence(sentence,ident)
    with ClauseSentence 
    with EstimateSentence{

   def this (sentence : MorfSentence) =  this(sentence.morfWords,sentence.ident,List[Segment]())
   
   def this (segments : List[Segment], ident : String) = 
               this(segments.map(t => t.words.map(w => Word.createMorfWord(w))).flatten,ident,segments)
   
   lazy val estimatedSegments = this.estimateClause(this.taggedSegments)
   
   val estimationOfClause : Int = {
        this.countEstimate(this.taggedSegments.map(t => t.analyzed).toList,false, false,false, 0)
    }
   
    var clauseEstimateSegments = if (levelWithSegments.isEmpty) {
      this.estimateClause(this.estimatedSegments.map(t => new TaggedSegment(t)).toList)
    }
    else {
       this.estimateClause(levelWithSegments.map(p => new TaggedSegment(p)).toList)
    }
   
  val clause = clauseEstimateSegments.zipWithIndex.map(s => (s._2, s._1.clause)).toList.groupBy( f => f._2).map(f => (f._1, f._2.map(t => t._1)))
  val countOfClause = clause.map(f => f._1).toList.max
  
  override def toString =  this.clauseEstimateSegments.map(t => t.clause.toString + " " + t.level.toString +" "+t.getStartNewClause.toString + " \n ").toList.reduce(_ + _)

  override val getEstimateSegments = this.estimatedSegments
  val getTestList    = this.getEstimateSegments.zipWithIndex.map(s => (s._2, s._1.clause)).toList
  override val getClause    = this.getEstimateSegments.zipWithIndex.map(s => (s._2, s._1.clause)).toList.groupBy( f => f._2).map(f => (f._1, f._2.map(t => t._1)))
  override val getEstimationOfCountClause : Int = this.countEstimate(this.estimatedSegments.map(segment => BaseSegment.createInfoSegment(segment)).toList, false, false,false, 0)
  override val getCountOfClause :Int = this.getEstimateSegments.map(f => f.clause).toList.max
   override val getIdent : String = this.ident
    
   override def TransformXml : Node = new XmlSentence(this.clauseEstimateSegments).TransformXml
  
  
  def applyMatches(matches : List[MatchEffect] ) = {
     this.clauseEstimateSegments.zipWithIndex.foreach(f => {
       val matchEffect = matches.filter(p => p.effectOnIndex == f._2)
       if (!matchEffect.isEmpty) {
         f._1.setClause(matchEffect.head.levelNum)
       }
     })  
  }
}