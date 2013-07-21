package common

trait ITag {
  
}
class Tag(val tag:String) extends ITag  {

  lazy val simpleTag = { 
            val (left, right) = tag.splitAt(1)
            (left + right.tail).toString
  }
  
  def isBoundary : Boolean = compare(0, "B") || compare(0, "C") 
  
  def haveActiveVerb : Boolean = compare(2,"C")
  
  def haveSubflag : Boolean = !compare(4,"X")
  
  def haveSimpleConj : Boolean = compare(5,"P") || compare(5,"J")
  
  def haveComma : Boolean = compare(5,"P")
  
  def interpunction : Boolean = compare(5,"Z")
  
  def haveDash : Boolean = compare(5,"D")
  
  def haveCordConj : Boolean = compare(5,"J")
  
  def Level : Int = {
                if (this.isEmpty) -1 
                else {
                  try {
                  this.tag.substring(1,2).toInt
                  }
                  catch {
                   case _ => { 
                              // println(this.tag) 
                              -1  
                             }
                  }
                }
               }
 def compare(pos : Int, comp : String) : Boolean = 
  {
   if (this.isEmpty) false
   else this.tag.substring(pos,pos+1) == comp
  }
  
  def isEmpty = this.tag.isEmpty
 override def toString : String = tag
 
 def addLevelToSimpleTag(l : Int) : Tag = {
     val (left, right) = tag.splitAt(1)
     new Tag((left + l.toString + right.tail).toString)
  }
}

// WithoutLevel
class SimpleTag(tag : String) extends ITag {
  
}