package statistic.Models

import statistic.BaseModel.ConditionalClauseModelTrain
import statistic.BaseModel.ConditionalModel

trait ConditionalClauseModel extends ConditionalClauseModelTrain with ConditionalModel  {

  lazy val probabilities = this.learnProbabilities
  
 lazy val countTags : Int = (this.getClauseTags.flatten.groupBy(f => f._2.simpleTag).toList.length + 1) * 10

 lazy val countLevels = this.getClauseTags.flatten.groupBy(f => f._2.Level).map(f => (f._1,f._2.length))
 
 
 var lamba0 : Double = (1).doubleValue / 4
 var lamba1 : Double = lamba0
 var lamba2 : Double = 1 - (lamba1)
 

 override def getProbabilities = this.probabilities 
  
  override def getLamba(index : Int) : Double = {
   if (index == 0) lamba0
   else if (index == 1) lamba1
   else 0
 }
 
 override def getCountTags : Int = this.countTags
 
}