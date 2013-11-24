package DataObjects

import common.segment.Segment
import Xml.XmlWritable

trait EstimateSentence extends XmlWritable {
 val getEstimateSegments : List[Segment]
 val getClause : Map[Int,List[Int]]
 val getEstimationOfCountClause : Int
 val getCountOfClause : Int
 val getIdent : String
 
 
}