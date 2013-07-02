package common.Interfaces

import common.segment.TaggedSegment
import common.segment.Segment

trait ISimpleClause {
  val tags : List[String]
  override def equals(x : Any) : Boolean = x match {
    case x : List[String] => this.compare(x,this.tags)
    case x : List[Segment] => this.compare(x.map(s => (new TaggedSegment(s)).GetTagString), this.tags)
    case _ => false
  }
  
  protected def compare(x : List[String], y: List[String]) : Boolean = {
    if (x.isEmpty || y.isEmpty) {
      false
    }
    else{
      compare(x.tail,y.tail)
    }
  }
}