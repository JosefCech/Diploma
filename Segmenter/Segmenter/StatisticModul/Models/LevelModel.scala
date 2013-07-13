package StatisticModul.Models


import common.Tag
import statistic.Models.UnigLevelModel

object LevelModel extends UnigLevelModel with LoaderTags {
 
   val simpleTags : List[List[Tag]] = this.LoadTags
    
   override val getSimpleTags = this.simpleTags
   override val toString : String =  super.toString
   
}