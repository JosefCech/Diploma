package common

class Tag(val tag:String) {

  def isBoundary : Boolean = compare(0, "B") || compare(0, "C") 
  
  def haveActiveVerb : Boolean = compare(2,"C")
  
  def haveSubflag : Boolean = !compare(4,"X")
  
  def haveSimpleConj : Boolean = compare(5,"P") || compare(5,"J")
  
  def haveCordConj : Boolean = compare(5,"J")
  
  def Level : Int = {
                if (this.isEmpty) -1 
                else this.tag.substring(1,2).toInt
               }
 def compare(pos : Int, comp : String) : Boolean = 
  {
   if (this.isEmpty) false
   else this.tag.substring(pos,pos+1) == comp
  }
  
  def isEmpty = this.tag.isEmpty
 override def toString : String = tag
}