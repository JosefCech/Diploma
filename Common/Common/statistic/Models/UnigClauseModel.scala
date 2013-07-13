package statistic.Models

import statistic.BaseModel.ClauseModelTrain
import statistic.BaseModel.UnigModel

trait UnigClauseModel extends ClauseModelTrain with UnigModel {
lazy val probabilities = this.learnProbabilities
  
 lazy val countTags : Int = (this.getClauseTags.flatten.groupBy(f => f._2.simpleTag).toList.length + 1) * 10  
 
 var lamba0 : Double = (1).doubleValue / 9
 var lamba1 : Double = 1 - lamba0
 
 override def getProbabilities = this.probabilities 
  
  override def getLamba(index : Int) : Double = {
   if (index == 0) lamba0
   else if (index == 1) lamba1
   else 0
 }
 
 override def getCountTags : Int = this.countTags
}