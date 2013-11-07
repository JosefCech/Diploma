package statistic

import common.Tag 

import statistic.BaseModel.UnigModel
import statistic.BaseModel.ConditionalModel

trait Viterbi {

  val getLevelModel :  UnigModel
  val getConditionModel : ConditionalModel
  val getAllStates : List[Int] = (0 to 9).toList
  
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
          paths.sortBy(f => f._1).toList.reverse.takeWhile(p => p._1 * maxGrow > maxActual * minGrow).toList.take(100)
        }
        else
        {
          // za��nut�
          paths.take(100)
        }
        
      }
   
      var actualUsedStates  = prunePaths.map(f => f._2.head).toSet.toList
  
      val actualTag = sentence.head
      
   /*   println(actualTag)
      println(maxActual)
      println(minActual)
      println(maxActual * minGrow)
      println(minActual * maxGrow)
      println(prunePaths.length)
      println(paths.length)
      println(actualUsedStates.length)
      */
      // override maxEdges(actualStates : List(Int,(Tag,Tag))) 
      
      val maxEdges = this.getMaxEdges(actualUsedStates, actualTag)
     /*  println(maxEdges.length)
       println(maxEdges)*/
      // add new part of path

      
      // override getNewPaths(prunePaths, maxEdges) : List[(Double, List[(Int, (common.Tag, common.Tag))])]
      val newPaths = this.getNewPaths(prunePaths, maxEdges, actualTag)
  
      getBestPath(sentence.tail, newPaths)
     
   }
  }
  
  def getBestPath(sentence : List[Tag]) : List[Int] = {
    val double = this.getBestPath(sentence,List((1.0,List((-1,(new Tag("*"),new Tag("*")))))))
    // list of levels
    double._2.reverse.tail.map(f => f._1)
  }
  
  def getMaxEdges(actualUsedStates : List[(Int,(Tag,Tag))], actualTag  : Tag) : List[(Int, Int, Double)]
  def getNewPaths(prunePaths: List[(Double, List[(Int, (common.Tag, common.Tag))])], maxEdges : List[(Int, Int, Double)] , actualTag : Tag ): List[(Double, List[(Int, (common.Tag, common.Tag))])] 
}