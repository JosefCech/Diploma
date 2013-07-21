package StatisticModul.Models


import common.Tag
import statistic.Models.UnigLevelModel

object LevelModel extends UnigLevelModel with LoaderTags {
 
   val simpleTags : List[List[Tag]] = this.LoadTags
    
   override val getSimpleTags = this.simpleTags
   override val toString : String =  super.toString
   
   override def getObserve(previous : Int, actual : Int ) = actual
}

object LevelModelDiff extends UnigLevelModel with LoaderTags {
 
   val simpleTags : List[List[Tag]] = this.LoadTags
    
   override val getSimpleTags = this.simpleTags
   override val toString : String =  super.toString
   
   override def getObserve(previous : Int, actual : Int ) = previous-actual
}