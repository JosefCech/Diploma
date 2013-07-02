package common

class Tag(val tag:String) {

  def isBoundary : Boolean = compare(0, "B") || compare(0, "C") 
  
  def haveActiveVerb : Boolean = compare(2,"C")
  
  def haveSubflag : Boolean = !compare(4,"-")
  
  def Level = this.tag.substring(1,2)
  
  private def compare(pos : Int, comp : String) : Boolean = 
  {
   this.tag.substring(pos,pos+1) == comp
  }
}