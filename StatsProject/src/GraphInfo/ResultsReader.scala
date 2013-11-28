package GraphInfo

import scala.xml.Node
import Xml.Reader

class ResultsReader(file : String ) extends Reader(file) {
  
  def GetResultData() : List[(String,List[(String,String)])] = {
    val data : Node =  super.GetData
    this.getChildNodesWithAtrib(data)
    
  }
   
}