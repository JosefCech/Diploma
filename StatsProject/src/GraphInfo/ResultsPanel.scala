package GraphInfo

import scala.swing.Panel
import java.io.File
import scala.swing.Orientation
import scala.swing.Label
import Xml.Reader
import javax.swing.BoxLayout
import scala.swing.GridPanel
import javax.swing.border.EmptyBorder
import scala.swing.BoxPanel

class RowData(val label : String, val succes : Double, val wholeCount : Int, val rightCount : Int)
{
 def toLabels : List[Label] = List[Label](
    new Label(label),
    new Label(succes.toString),
    new Label(rightCount.toString),
    new Label(wholeCount.toString)
 )
}

class ResultsPanel(val indexResult : String, val indexType: String)  {

  def GetSuffix : String = indexResult match {
    case "StatisticClause" | "RuleClause" => "-withLevel"
    case "RuleLevel" | "StatisticLevel" => ""
      
  }
  
  def GetPanel : Panel = {
    val resultFilePath = "../Result/" +  this.indexResult + "-" + this.indexType + this.GetSuffix + ".xml";
    val resultFile = new File(resultFilePath)
    if (resultFile.exists) {
      val data = this.ReadData(resultFile)
      val headers = this.Headers;
      val resultPanel = new GridPanel(data.size.toInt + 1,4)
      {
          border = new EmptyBorder(10, 10, 10, 10)
          headers.foreach(h => this.contents += h)
          data.foreach(r => r.toLabels.foreach(l => this.contents += l))
      }
      return resultPanel
    }
    else {
      val resultPanel = new BoxPanel(Orientation.Horizontal)
      {
        contents += new Label("File not found " + resultFilePath )
      }
      return resultPanel
    }
    
  }
  
  
  def ReadData(file : File) : List[RowData] = {
    val data = new ResultsReader(file.getAbsolutePath).GetResultData
   data.map(f => {
      val name = f._1
      val right : Double = f._2.filter(p => p._1 == "right").head._2.toDouble
      val rightCount : Int = f._2.filter(p => p._1 == "wholeCount").head._2.toInt
      val wholeCount : Int = f._2.filter(p => p._1 == "rightCount").head._2.toInt
     new RowData(name, right, wholeCount, rightCount)
   }).toList
  }
  
  def Headers() : List[Label] = List[Label](
   new Label("Type"),
   new Label("Succesfull"),
   new Label("Whole count of elements"),
   new Label("Right estimate of elements")
  )
    

}