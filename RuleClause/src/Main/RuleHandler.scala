package Main

import common._

object RuleHandler {

   def  updateLevels(addLevel : Int, segments : List[(Int,AnalyzedSegment)] , acc : List[(Int,AnalyzedSegment)] ) : List[(Int,AnalyzedSegment)] = {
     if (segments.isEmpty) acc.reverse
     else {
        val head = segments.head
        updateLevels(addLevel,segments.tail,(head._1 + addLevel,head._2) :: acc)
     }
   }
  /*
   def apply(rule : Rule , segments : List[(Int,AnalyzedSegment)]) : List[(Int,AnalyzedSegment)] = rule match {
     case rule : ExcactLevelRule =>  null
     case rule : MultiplyLevelRule => null
     case rule : AnotherRule => null
     */
   }
