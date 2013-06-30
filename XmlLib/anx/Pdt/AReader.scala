package Pdt
  import Xml.XmlReader
import common.Tree
import common.NodeInTree
import common.TreeNode
import common.Root
import scala.xml.Node
import java.io.File


object AReader extends XmlReader {

  def CreateTrees(file : File) : List[Tree] = {
    try {
		   val source  = scala.xml.XML.loadFile(file)
		   val roots = (source\\"trees"\"LM").toList
		   roots.map(t => AReader.CreateTree(t)).toList
    }
   catch  {
     case e : Exception => { 	println(file)
    	 						List[Tree]()
     						}
     }
  }
  def CreateTree(node : scala.xml.Node) : Tree = {
    val ident = (node\"s.rf").toList.head.text.split("#").tail.head 
    if ((node\"children").toList.isEmpty) {
      new Tree(new Root(ident))
    }
    else {
      val children = (node\"children"\"LM").toList.map(t => AReader.CreateNode(t))
      new Tree(new Root(ident,children))
    }
  }
  
  def CreateNode(node : scala.xml.Node) : NodeInTree = {
	   val ident = (node\"m.rf").toList.head.text.split("#").tail.head
	   val ord = (node\"ord").toList.head.text.toInt
	   val afun = (node\"afun").toList.head.text
	   val numClause = this.GetClauseNum(node)
	   if ((node\"children").toList.isEmpty) {
	      new  NodeInTree(ident,ord,afun,numClause)
	   }
	   else {   
	     val children = (node\"children"\"LM").toList.map(t => AReader.CreateNode(t))
	     new  NodeInTree(ident,ord,afun,children,numClause)
	   }	   
   }
  
  def GetClauseNum(node : scala.xml.Node) : Int = {
    // 0 - is coord
    // num 
    // -1 doesnt exist
    val clauseNum = (node\"clause_number");
    
    if (clauseNum.isEmpty) -1
    else clauseNum.toList.head.text.toInt  
   
   
  } 

}