package StatisticModul.Models

import statistic.Models.UnigClauseModel
import common.Tag

object ClauseModel extends UnigClauseModel with LoaderTags {
 
   val clauseTags : List[List[(Int,Tag)]] = this.LoadTagsWithClause
    
   override val getClauseTags = this.clauseTags
   override val toString : String =  super.toString
   
     override def getObserve(previous : Int, actual : Int ) = actual
   
}
