package GraphInfo

import scala.swing._
import scala._

class BaseInfoContainer(string : String) extends scala.swing.Panel {
  
  def baseInfo  = new Stats.BaseInfo(string)
  
  def errors = baseInfo.errors
  def countSegments = baseInfo.countSegments
  def noVerbSegments = baseInfo.noVerbSegments
  def maxLevel = baseInfo.maxLevel
  
  def panel = new FlowPanel(FlowPanel.Alignment.Center)(
        new FlowPanel(FlowPanel.Alignment.Center)( new Label(" Chyby segementace "), new Label(errors.size.toString)),
		new FlowPanel(FlowPanel.Alignment.Center)( new Label(" Poèet segmentù "), new Label(countSegments.toString)),
		new FlowPanel(FlowPanel.Alignment.Center)( new Label(" Poèet segmentù bez slovesa "), new Label(noVerbSegments.size.toString)),
		new FlowPanel(FlowPanel.Alignment.Center)( new Label(" Nejhloubší segment "), new Label(maxLevel.toString)),
		new ListView(baseInfo.clauseEstimatedCount)
  )
  
  def box =  new BoxPanel(Orientation.Vertical) {
         contents += new FlowPanel(FlowPanel.Alignment.Center)( new Label(" Chyby segementace "), new Label(errors.size.toString))
		 contents += new FlowPanel(FlowPanel.Alignment.Center)( new Label(" Poèet segmentù "), new Label(countSegments.toString))
		 contents += new FlowPanel(FlowPanel.Alignment.Center)( new Label(" Poèet segmentù bez slovesa "), new Label(noVerbSegments.size.toString))
		 contents += new FlowPanel(FlowPanel.Alignment.Center)( new Label(" Nejhloubší segment "), new Label(maxLevel.toString))
		 //contents += new FlowPanel(new Label(" Poèet vìt s clausemi na základì aktivního slovesa "),new ListView(baseInfo.clauseEstimatedCount))
         contents += new FlowPanel(new Label(" Poèet segmentù s vyšším levelem vs subflags "),new ListView(baseInfo.subFlagsStats))
         border = Swing.EmptyBorder(30, 30, 10, 30)
        }
  

}