package DataObjects

import common.segment.Segment

trait EstimateSentence {
 val getEstimateSegments : List[Segment]
 val getClause : Map[Int,List[Int]]
 val getEstimationOfCountClause : Int
 val getCountOfClause : Int
}