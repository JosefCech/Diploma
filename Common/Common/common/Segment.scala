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
  protected var _level : Level  =  new Level(-1,-1)
  protected var _clause : Int  = -1
  def level =  _level
  def clause = _clause
  
  
 def ToString : String  = words.map(s => s match {
    case s : Word => s.form
  	}
   ).mkString(" ") 
   
  def level_=(l : Level) {
     this._level = l
  } 
  def UpdateLevel(u : Int) : Unit = this.level += u 
  def SetLevel(l : Int) : Unit = {
     this.level_=(new Level(l,l))
  }
  
  protected def clause_=(c : Int){
    _clause = c
  }
  
  def SetClause(c : Int) : Unit = {
    this.clause_=(c)
  }
}


class BaseSegment(val data : List[Any] , lmin : Int , lmax : Int) extends Segment {
 
  def this(data : List[Any], level : Int) = this(data,level,level)
  
  def this(data : List[Any]) = this(data,-1,-1)
  
  this.level = new Level(lmin,lmax) 
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

class Boundary( data : List[Any] , lmin : Int , lmax : Int )
extends BaseSegment(data, lmin, lmax) {
  def this(data : List[Any], l: Int) = this(data,l,l)
  def this(data : List[Any]) = this(data,-1)	
}



class PureSegment(data : List[Any] , lmin : Int , lmax : Int ) 
extends BaseSegment(data, lmin, lmax) {
 def this(data : List[Any], l: Int) = this(data,l,l)
  def this(data : List[Any]) = this(data,-1,-1)
}
