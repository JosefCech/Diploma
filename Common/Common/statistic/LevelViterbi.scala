package statistic

import common.Tag

trait LevelViterbi  extends Viterbi {
 
   override def getMaxEdges(actualUsedStates : List[(Int,(Tag,Tag))], actualTag  : Tag) : List[(Int, Int, Double)] = 
   {
     this.getAllStates.map(nextNode => {
      val maxNext =  actualUsedStates.map(actualNode => {
           val levelProb = this.getLevelModel.getProbability(nextNode, actualTag.simpleTag) 
           val conditionProb = this.getConditionModel.getProbability(nextNode, actualNode._2._1.tag, actualNode._2._2.tag)
           val tripple = (actualNode._1,nextNode,levelProb * conditionProb )
           tripple
        }).groupBy(f => f._3).maxBy(f => f._1)._2
        maxNext
      }).flatten
   }
  def getNewPaths(prunePaths: List[(Double, List[(Int, (common.Tag, common.Tag))])], maxEdges : List[(Int, Int, Double)], actualTag : Tag )
  : List[(Double, List[(Int, (common.Tag, common.Tag))])] = 
  {
    prunePaths.map( f => {
        maxEdges.filter(a => a._1 == f._2.head._1).toList.map( newEdge => {
        
        val newTag = actualTag.addLevelToSimpleTag(newEdge._2)
        val newPath = (f._1*newEdge._3, (newEdge._2, (newTag ,f._2.head._2._1)) :: f._2)
        
        newPath
        })
      }).flatten.toList
  }
}