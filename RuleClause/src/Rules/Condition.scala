package Rules

/**
 *  trait  Rule should represented single different rule in each instance
 *    
 **/

class Condition(val segmentTemplate : List[SegmentTemplate])
{
	def isEmpty : Boolean = segmentTemplate.isEmpty
	override def toString : String = segmentTemplate.map(f => f.toString).mkString("\n \n")
}



