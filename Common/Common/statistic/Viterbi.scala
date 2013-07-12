package statistic

import common.Tag

trait Viterbi {

  val getLevelModel :  LevelSegmentModel
  val getConditionModel : SegmentConditionalModel
  val getAllStates : List[Int]
  
  def getBestPath(sentence : List[Tag], paths : List[(Double, List[(Int,(Tag,Tag))])]) : (Double, List[(Int,(Tag,Tag))]) =
  {
   if (sentence.isEmpty)
   {
     paths.maxBy(f => f._1)
   }
   else {
      val maxActual = paths.map(p => p._1).max
      val minActual = paths.map(p => p._1).min
      val maxGrow = sentence.length * getLevelModel.getProbMax
      val minGrow = sentence.length * getLevelModel.getProbMin
      val prunePaths = 
      {
        if (maxActual * minGrow > minActual * maxGrow)
        {
          paths.sortBy(f => f._1).toList.dropWhile(p => p._1 * maxGrow < maxActual * minGrow).toList
        }
        else
        {
          paths
        }
        
      }
   
      var actualUsedStates  = paths.map(f => f._2.head).toSet.toList
  
      val actualTag = sentence.head
 
      val maxEdges = this.getAllStates.map(nextNode => {
      val maxNext =  actualUsedStates.map(actualNode => {
           val levelProb = this.getLevelModel.getProbability(nextNode, actualTag.simpleTag) 
           val conditionProb = this.getConditionModel.getProbability(nextNode, actualNode._2._1.tag, actualNode._2._2.tag)
           val tripple = (actualNode._1,nextNode,levelProb * conditionProb )
           tripple
        }).groupBy(f => f._3).maxBy(f => f._1)._2
        maxNext
      }).flatten
         
      // add new part of path

      
      val newPaths = prunePaths.map( f => {
        maxEdges.filter(a => a._1 == f._2.head._1).toList.map( newEdge => {
        
        val newTag = actualTag.addLevelToSimpleTag(newEdge._2)
        val newPath = (f._1*newEdge._3, (newEdge._2, (newTag ,f._2.head._2._1)) :: f._2)
        
        newPath
        })
      }).flatten.toList
  
      getBestPath(sentence.tail, newPaths)
     
   }
  }
}