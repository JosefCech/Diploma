package StatisticModul.Models

import common.Tag

object ConditionalClauseModel extends statistic.Models.ConditionalClauseModel with LoaderTags {
 
   val clauseTags : List[List[(Int,Tag)]] = this.LoadTagsWithClause
    
   override val getClauseTags = this.clauseTags
   override val toString : String =  super.toString
}
