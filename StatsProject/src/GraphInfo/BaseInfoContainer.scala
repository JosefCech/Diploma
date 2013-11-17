package GraphInfo

import scala.swing._
import scala._

class BaseInfoContainer(string : String) extends scala.swing.Panel {
  
  def baseInfo  = new Stats.BaseInfo(string)
  
 //val errors = baseInfo.errors
/* val countSegments = baseInfo.countSegments
 val noVerbSegments = baseInfo.noVerbSegments
 val maxLevel = baseInfo.maxLevel
 */
  /*
  def panel = new FlowPanel(FlowPanel.Alignment.Center)(
        new FlowPanel(FlowPanel.Alignment.Center)( new Label(" Chyby segementace "), new Label(errors.size.toString)),
		new FlowPanel(FlowPanel.Alignment.Center)( new Label(" Počet segmentů "), new Label(countSegments.toString)),
		new FlowPanel(FlowPanel.Alignment.Center)( new Label(" Počet segmentů bez slovesa "), new Label(noVerbSegments.size.toString)),
		new FlowPanel(FlowPanel.Alignment.Center)( new Label(" Nejhloubší segment "), new Label(maxLevel.toString)),
		new ListView(baseInfo.clauseEstimatedCount)
  )
  
  def box =  new BoxPanel(Orientation.Vertical) {
         contents += new FlowPanel(FlowPanel.Alignment.Center)( new Label(" Chyby segementace "), new Label(errors.size.toString))
		 contents += new FlowPanel(FlowPanel.Alignment.Center)( new Label(" Počet segmentů "), new Label(countSegments.toString))
		 contents += new FlowPanel(FlowPanel.Alignment.Center)( new Label(" Počet segmentů bez slovesa "), new Label(noVerbSegments.size.toString))
		 contents += new FlowPanel(FlowPanel.Alignment.Center)( new Label(" Nejhloubší segment "), new Label(maxLevel.toString))
		 contents += new FlowPanel(new Label(" Počet vět s clausemi na základě aktivního slovesa "),new ListView(baseInfo.clauseEstimatedCount))
         contents += new FlowPanel(new Label("Četnost subflagů "),new ListView(baseInfo.sublflagsDetail))
         
         border = Swing.EmptyBorder(30, 30, 10, 30)
        }
  */
  def box =  new BoxPanel(Orientation.Vertical) {
        contents += new FlowPanel(FlowPanel.Alignment.Center)( new Label(" Počet segmentů "), new Label("test"))
		 
         
         border = Swing.EmptyBorder(30, 30, 10, 30)
        }
  

}