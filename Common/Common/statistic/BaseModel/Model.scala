package statistic.BaseModel

import common.Tag

trait BaseLevelModel {
 val getSimpleTags  : List[List[Tag]]
}

trait BaseClauseModel {
  val getClauseTags : List[List[(Int,Tag)]]
}