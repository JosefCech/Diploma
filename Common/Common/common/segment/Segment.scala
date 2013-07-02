package common.segment


import common.MorfWord
import common.Word


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

trait ISegment {
  
}

trait Segment {
  def words : List[Word]
  protected var _level : Level  =  new Level(-1,-1)
  protected var _clause : Int  = -1
  protected var _startNewClause = false
  def level =  _level
  def clause = _clause
  
  
 override def toString : String  = words.map(s => s match {
    case m : MorfWord => m.form + "("+ m.tag +")"
    case s : Word => s.form
    
  	}
   ).mkString(" ") 
   
  def level_=(l : Level) {
     this._level = l
  } 
  def updateLevel(u : Int) : Unit = this.level += u 
 
  def setLevel(l : Int) : Unit = {
     this.level_=(new Level(l,l))
  }
  
  def clause_=(c : Int){
    _clause = c
  }
  
  def setClause(c : Int) : Unit = {
    this.clause_=(c)
  }
  
  def setStartNewClause() : Unit = {
    this._startNewClause = true
  }
  
  def getStartNewClause : Boolean = this._startNewClause
}


class BaseSegment(val data : List[Any] , lmin : Int , lmax : Int, val startNewClause : Boolean) extends Segment {
 
  def this(data : List[Any], level : Int,  startNewClause : Boolean) = this(data,level,level,startNewClause)
  
  def this(data : List[Any], level : Int ) = this(data,level,level,false)
  
  
  def this(data : List[Any]) = this(data,-1,-1,false)
  
  
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

class Boundary( data : List[Any] , lmin : Int , lmax : Int  )
extends BaseSegment(data, lmin, lmax, false) {
  def this(data : List[Any], l: Int) = this(data,l,l)
  def this(data : List[Any]) = this(data,-1)	
}

class PureSegment(data : List[Any] , lmin : Int , lmax : Int , startNewClause : Boolean ) 
extends BaseSegment(data, lmin, lmax , startNewClause) {
 def this(data : List[Any], l: Int, startNewClause : Boolean) = this(data,l,l,startNewClause)
 
  def this(data : List[Any], l: Int) = this(data,l,l,false)
  
  def this(data : List[Any]) = this(data,-1,-1,false)
}

