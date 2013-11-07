package Rules

import common.RuleWord

class SegmentTemplate(val tags : String, val typeSegment : String, val words : List[RuleWord], val isGroup : Boolean ) 
{
 def this() = this("","",List[RuleWord](),false) 
  
 def isEmpty : Boolean = tags.isEmpty && typeSegment.isEmpty && words.isEmpty && isGroup == false
 
 override def toString : String = "Tags :" + tags +"\n" +
                         "Type " + typeSegment +"\n" +
                         "Group " + isGroup.toString +"\n" +
                         words.map(t => t.toString).mkString(" \n\t")
}
