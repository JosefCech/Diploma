package common


abstract class Segment {
  def words : List[Word]
  def level : Int
  
  def ToString : String  = words.map(s => s match {
    case s : Word => s.form
  	}
   ).mkString(" ") 
   
  
}

class BaseSegment(val data : List[Any] ,  val level : Int ) 
	extends Segment {
   override def words : List[Word] =
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
	                                       
	def haveSubFlag = !words.filter(p => p match {case p : MorfWord => p.isSubFlag 
												  case p : Word => false
	                                             }
	                                       ).isEmpty
}

class Boundary(  data : List[Any] , level : Int = 0) 
extends BaseSegment(data,level) {
 
  
}



class PureSegment(data : List[Any] , level : Int ) 
extends BaseSegment(data,level) {
		

}
