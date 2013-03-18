package common

import common.Interfaces._

class SimpleClause(val tags : List[String]) extends ISimpleClause {
 override def toString : String = {
   tags.foldLeft("")((s,t) => s.concat("-" + t))
 }
}