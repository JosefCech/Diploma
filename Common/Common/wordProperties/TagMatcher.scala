package wordProperties
import common._
object TagMatcher {
  
	def Match(word : MorfWord , tag : String ) : Boolean = {
	   Match(word.tag,tag)
	}
	
	def Match(tag1 : String, tag2 : String) : Boolean = {
	  def MatchPosition(tag1 : String, tag2 : String) : Boolean = {
	     if (tag1.isEmpty || tag2.isEmpty) true
	     else {
	     val p1 = tag1.head
	     val p2 = tag2.head
	     if ((p1 == p2) || (p2 == '_') || (p1=='_'))
		     {
		      MatchPosition(tag1.tail,tag2.tail)
		     }
		 else
		     {
		       false 
		     }
	     }
	  } 
	  ExactMatch(tag1,tag2) || MatchPosition(tag1,tag2)
	}
	
	def ExactMatch(tag1 : String , tag2 : String) : Boolean = {
	  tag1 == tag2
	}

	def MatchSet(tag : String, set : List[String]) : Boolean = {
	   if (set.isEmpty) false
	   else if (Match(tag,set.head)) true
	   else MatchSet(tag,set.tail)
	}
	
	def NoMatch(tag1 : String, tag2 : String) : Boolean = {
	  !Match(tag1,tag2)	  
	}
	
	
	def MatchSetExcept(words : List[MorfWord] , set : List[String], except : List[String]) : List[MorfWord] = {
	  words.filter(p => MatchSet(p.tag, set)).filterNot(p => MatchSet(p.tag,except)).toList
	}  
}