package GraphInfo

import Stats._
import scala.swing._
import scala.swing.event._
import java.awt.CardLayout
import javax.swing.border.EmptyBorder


object ResultsInfo extends SimpleSwingApplication {
  
 class ComboBoxItem(val value : String, val visibleName : String) 
 {
	 override def toString =new String(visibleName.getBytes(), "UTF-8")  
	 def getIndex = value
 }
  def top = new MainFrame {
		title = "Results from :"
		val resultsType = List[(String,String)](
		  ("StatisticClause", "Clause / Statistic estimation"),
		  ("RuleClause", "Clause / Rule estimation "),
		  ("RuleLevel", "Level / Rule estimation"),
		  ("StatisticLevel", "Level / Statistic estimation")
		)
		
		val dataType = List[(String,String)](
		  ("Develop", "Development data"),
		  ("Heldout", "Heldout data"),
		  ("Test", "Test data"),
		  ("TestClause", "Test clause data")
		)
		
		val usingMyLevel = List[(String,String)](
		 ("withLevel", "Use level from golden data"),
		 ("withoutLevel", "Use integrated segmenter")
		 
		)
		
		val resultsTypeCombo = new swing.ComboBox(resultsType.map(r => new ComboBoxItem(r._1,r._2)).toList)
		val dataTypeCombo = new swing.ComboBox(dataType.map(r => new ComboBoxItem(r._1,r._2)).toList)
		val levelCombo = new swing.ComboBox(usingMyLevel.map(r => new ComboBoxItem(r._1,r._2)).toList)
		val buttonView = new Button("View results")
		
		val mainLayout = new BoxPanel(Orientation.Horizontal){
		 contents += new Label("Result type : ")
		 contents += resultsTypeCombo
		 contents += new Label("Data type : ")
		 contents += dataTypeCombo

		 contents += new Label("Level : ")
		 contents += levelCombo
		 
		 contents += buttonView
		}
		
	
			contents = new BorderPanel(){
			border = new EmptyBorder(10,10,10,10)
			listenTo(resultsTypeCombo.selection)
			listenTo(dataTypeCombo.selection)
			listenTo(buttonView)
			reactions += {
			   // case SelectionChanged(`resultsTypeCombo`) => Dialog.showMessage(this, "Data changed. " + resultsTypeCombo.selection.item.toString)
			    case ButtonClicked(buttonView) => {
			      val resultsPanel = new ResultsPanel(resultsTypeCombo.selection.item.getIndex, dataTypeCombo.selection.item.getIndex)
			      add(resultsPanel.GetPanel, BorderPanel.Position.Center)
			      Dialog.showMessage(this, "Data changed. " + resultsTypeCombo.selection.item.getIndex + " " +  dataTypeCombo.selection.item.getIndex )
			      pack
			    }
			}
						
			add(mainLayout, BorderPanel.Position.North)
		}
  	}
  
  
  
	
}