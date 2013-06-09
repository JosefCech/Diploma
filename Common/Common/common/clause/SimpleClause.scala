package common.clause

import common.Interfaces.ISimpleClause

class SimpleClause(val tags : List[String]) extends ISimpleClause {
 override def toString : String = {
   tags.foldLeft("")((s,t) => s.concat("-" + t))
 }
}