package statistic.BaseModel

import common.Tag

trait BaseLevelModel {
 val getSimpleTags  : List[List[Tag]]
  def getObserve(previous : Int, next : Int ) : Int
}

trait BaseClauseModel {
  val getClauseTags : List[List[(Int,Tag)]]
   def getObserve(previous : Int, next : Int ) : Int
}