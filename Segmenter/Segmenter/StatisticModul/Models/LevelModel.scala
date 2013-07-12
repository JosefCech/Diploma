package StatisticModul.Models

import statistic.LevelSegmentModel
import common.Tag

class LevelModel extends LevelSegmentModel with LoaderTags {
 
   val simpleTags : List[List[Tag]] = this.LoadTags
    
   override val getSimpleTags = this.simpleTags
   override val toString : String =  super.toString
}