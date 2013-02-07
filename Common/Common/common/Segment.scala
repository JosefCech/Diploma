package common


class Level(min : Int , max : Int) {
 def this(level : Int) = this(level,level)
 
 def updateLevelMax(u : Int) : Level = new Level(min,List(min,max+u).max)
 def updateLevelMin(u : Int) : Level = new Level(List(0,min+u).max,max)
 def updateLevel(u : Int) : Level = new Level(List(0,min + u).max,List(0,max + u).min)

 def setIntervalLevel(min : Int, max : Int) : Level = new Level(min,max)
 def setLevel(level : Int) : Level = new Level(level,level)
 
 def isIntervalLevel = min < max
 
 def +=(level : Int) : Level = this.updateLevel(level)

 def getExactLevel : Int =  { 
   if (min == max) {
     min
   } 
   else {
     throw new ExceptionInInitializerError("is interval level")
   }
 }
 def isPossibleLevel(optionLevel : Int) = (optionLevel >= min) && (optionLevel <= max)
 
 override def toString : String = {
   if (min == max) {
     min.toString
   }
   else {
     "min : " + min.toString + " max : " + max.toString
   }
 }
}

trait Segment {
  def words : List[Word]
  var level : Level 
  
 def ToString : String  = words.map(s => s match {
    case s : Word => s.form
  	}
   ).mkString(" ") 
   
  def updateLevel(u : Int) : Unit = this.level += u 
  def setLevel(l : Int) : Unit = this.level = new Level(l)
}


class BaseSegment(val data : List[Any] , var level : Level) extends Segment {
  
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
	                                       
	def haveSubFlag = !words.filter(p => p match {case p : MorfWord => p.isSubFlag 
												  case p : Word => false
	                                             }
	                                       ).isEmpty
	                                       
	override def toString = words.foldLeft("")((a,f) => a + " " + f.form)                                      
  
}

class Boundary( data : List[Any] , level : Int = 0) 
extends BaseSegment(data,new Level(level)) {
  def this(data : List[Any]) = this(data,-1)	
  
}



class PureSegment(data : List[Any] , level : Int ) 
extends BaseSegment(data,new Level(level)) {
 def this(data : List[Any]) = this(data,-1)		

}
