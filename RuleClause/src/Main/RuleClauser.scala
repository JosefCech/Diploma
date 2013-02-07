package Main
import common.{Segment , AnalyzedSegment,ClauseSentence,Clause,Sentence}
import Rules.{AnalyzedSentence , RuleHandler, Effect}
import scala.{Enumeration}

object SegmentRelation extends Enumeration {
    type SegmentRelation = Value
    val NewSegment, JointWithPrevious , JoinWithNext = Value
 }


object RuleClauser {
  
 def analyzeSentence(sentence : Sentence) : ClauseSentence = {
   
   val segments = new AnalyzedSentence(sentence.segments).morfSegmentsIndexed
   val applyingRules = RuleHandler.applicableRules(segments)
   val applySegmentToClause = getRuleSegments(applyingRules)
   val applyGlobalEffect = getRuleLevel(applyingRules)
     
   println(applySegmentToClause)
   println(applyGlobalEffect)
   
   new ClauseSentence(List((0,List[Clause]())));
 }
     
   
 def getRuleSegments(applyingRules : List[(Int,Effect,Int)]) : List[(Int, SegmentRelation.Value)] = {
     def getRuleEffect(applyingRules : List[(Int,Effect,Int)], acc : List[(Int, Int, SegmentRelation.Value)] ): List[(Int, Int , SegmentRelation.Value)] = {
        if (applyingRules.isEmpty){
          acc
        }
        else {
          val headRule = applyingRules.head
          val indexSegment = headRule._1 + headRule._2.effectOn.toInt
          val usingSegment = {
            headRule._2.segment match {
              case "NewClause" => SegmentRelation.NewSegment
              case "JointWithNext" => SegmentRelation.JoinWithNext
              case "JoinWithPrevious" => SegmentRelation.JointWithPrevious
              case _ => SegmentRelation.NewSegment
            }
          }
          getRuleEffect(applyingRules.tail,(indexSegment,headRule._3,usingSegment) :: acc)
        }
    }
     val data = getRuleEffect(applyingRules,List[(Int,Int,SegmentRelation.Value)]())
     if (data.isEmpty){
       List[(Int, SegmentRelation.Value)]()
     }
     else {
     data.groupBy(f => f._2).maxBy(f => f._1)._2.map(f => (f._1,f._3))
     }
 }
 
 def getRuleLevel(applyingRules : List[(Int,Effect,Int)]): List[(Int, Int)]  = {
     
 def getRuleLevelAcc(applyingRules : List[(Int,Effect,Int)],  acc : List[(Int, Int, Int)] ): List[(Int,Int,Int)] = {
      if (applyingRules.isEmpty) {
        acc
      }
      else {
        val headRule = applyingRules.head
        val levelUpdate = headRule._2.level.toInt
        val priority = headRule._3
        getRuleLevelAcc(applyingRules.tail,(headRule._1,priority,levelUpdate) :: acc)
      }
    }
  val data = getRuleLevelAcc(applyingRules.filterNot(p => p._2.level == "ActualLevel"),List[(Int,Int,Int)]())
  if (data.isEmpty) {
    List[(Int, Int)]()
  } 
  else {
  data.groupBy(f => f._2).maxBy(f => f._1)._2.map(f => (f._1,f._3))
  }
 }
 
}
  
