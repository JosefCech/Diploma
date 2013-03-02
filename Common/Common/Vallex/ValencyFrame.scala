package Vallex


class ValencyFrame(items : Array[ValencyFrameItem]) {
 
  def this() = this(Array[ValencyFrameItem]())
  
  def addItem(caseItem : Int , obligathory : Boolean) : ValencyFrame = this.addItem(new ValencyFrameItem(caseItem,obligathory))
  
  def addItem(item : ValencyFrameItem) = new ValencyFrame(this.items ++ Array{item})
}