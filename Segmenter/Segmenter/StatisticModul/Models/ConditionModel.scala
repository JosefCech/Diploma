package StatisticModul.Models


import common.Tag
import statistic.Models.ConditionalLevelModel

object ConditionModel extends ConditionalLevelModel with LoaderTags {
 
   val simpleTags : List[List[Tag]] = this.LoadTags
    
   override val getSimpleTags = this.simpleTags
   override val toString : String =  super.toString
}
