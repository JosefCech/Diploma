package common

trait TreeNode {
  def WordIdent : String = this match {
    case c :  NodeInTree => c.Ident
    case _  => ""
  }
 
  def  SentenceIdent : String = this match {
   case c : Root => c.Ident
   case _  => ""
  }
  
  def GetChildren : List[TreeNode] = this match {
    case c : NodeInTree => c.Children
    case c : Root => c.Children
  }
  
  def IsRoot : Boolean = this match {
    case c : Root => true
    case _  => false
  }
  
 def GetAfun : String = this match {
   case c : NodeInTree => c.Afun
   case _  => ""
 }
 
 def GetOrdNum : Int = this match {
   case c : NodeInTree => c.Ord
   case r : Root => 0
   case _ => -1
 }
 
  def GetClauseNum : Int = this match {
   case c : NodeInTree => c.ClauseNum
   case _ => -1
 }
  
  def GetIdent : String = this match {
    case c : Root => c.Ident
    case c : NodeInTree => c.Ident
  }
 
}

class Root(val Ident : String, val Children : List[TreeNode]) extends TreeNode {
  
  def this(Ident : String) = this(Ident,List[TreeNode]())
  
  override def toString() : String =  {
   var string : String = "Root Node \n"
   string += "Ident : " + this.Ident + "\n"
   string 
   }
}

class NodeInTree(val Ident : String, val Ord : Int,val Afun: String, val Children : List[TreeNode], val ClauseNum : Int) extends TreeNode {
  def this (Ident : String, Ord : Int , Afun : String, ClauseNum: Int) = this(Ident, Ord, Afun, List[TreeNode](),ClauseNum)
  override def toString() : String = {
                  var string : String = "Inner Node \n"
                	  string += "Ident : " + this.Ident + "\n"
                      string += "Ord " + this.Ord.toString + "\n"
                      string += "Afun : " + this.Afun + "\n"
                      string += "ClauseNum :" + this.ClauseNum + "\n"
                      string
                  } 
}

class Tree(val root : TreeNode) {
 val ident = root.GetIdent
 
 val words : List[AWord] = {
   def CreateListWord(nodes : List[TreeNode], acc : List[AWord]) : List[AWord] =
   {
     if (nodes.isEmpty) return acc.sortBy(f => f.ord)
     else {
       val node = nodes.head 
       val aWord = new AWord(node.GetIdent,node.GetOrdNum, node.GetClauseNum)
       CreateListWord(nodes.tail ::: node.GetChildren, aWord :: acc)
     }
   }
   CreateListWord(this.root.GetChildren,List[AWord]())
 }
}