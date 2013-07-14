package StatisticModul.Models


import common.Tag


object ConditionalLevelModel extends statistic.Models.ConditionalLevelModel with LoaderTags {
 
   val simpleTags : List[List[Tag]] = this.LoadTags
    
   override val getSimpleTags = this.simpleTags
   override val toString : String =  super.toString
}
