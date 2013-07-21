package statistic.Models

import statistic.BaseModel.ConditionalLevelModelTrain
import statistic.BaseModel.ConditionalModel


trait ConditionalLevelModel extends ConditionalLevelModelTrain with ConditionalModel {
  
 lazy val probabilities = this.learnProbabilities
  
 lazy val countTags : Int = (this.getSimpleTags.flatten.groupBy(f => f.simpleTag).toList.length + 1) * 10

 lazy val countLevels = this.getSimpleTags.flatten.groupBy(f => f.Level).map(f => (f._1,f._2.length))
 
 
 var lamba0 : Double = (1).doubleValue / 10
 var lamba1 : Double = 10*lamba0 
 var lamba2 : Double = 1 - (lamba1 + lamba0)
 

 override def getProbabilities = this.probabilities 
  
  override def getLamba(index : Int) : Double = {
   if (index == 0) lamba0
   else if (index == 1) lamba1
   else 0
 }
 
 override def getCountTags : Int = this.countTags
 
}
