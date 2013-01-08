package common


abstract class Segment {
  def words : List[Word]
 
  
  def ToSentence() : String  = words.map(s => s match {
    case s : Word => s.form
  	}
   ).mkString(" ") 
   
  
  def ContainsSubcord() : Boolean = {
	   false
	 }
}

class PureSegment(val data : List[Any]) extends Segment {
	
	 def words : List[Word] =
			 data.map( f => f match 
		  					{
		  						case f : String => new Word(f)
		  						case f : (String,String) => new MorfWord(f._1,f._2)
		  					    case f : Word => f
		  					}
		  			)	
		  			
	def separators = words.takeWhile(p => p match {case p : MorfWord => p.isSeparator 
												 case p : Word => false
	                                             }
	                                       ).toList
	                                       
	def haveSubFlag = !words.filter(p => p match {case p : MorfWord => p.isSeparator 
												  case p : Word => false
	                                             }
	                                       ).isEmpty
}

class AnalyzedSegment(data : List[Any], val level : Int) extends PureSegment(data) {
  
    
}