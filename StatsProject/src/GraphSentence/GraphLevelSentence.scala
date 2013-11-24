package GraphSentence

import scala.swing.SimpleSwingApplication
import scala.swing.MainFrame
import scala.swing.Label
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.Orientation
import scala.swing.Swing
import scala.swing.GridPanel
import common.segment.Segment
import common.segment.PureSegment
import java.awt.Color
import java.awt.Dimension
import javax.swing.border.LineBorder
import scala.swing.FileChooser
import java.io.File
import scala.swing.event.ButtonClicked

object GraphLevelSentence extends SimpleSwingApplication {
  
 
  var sentenceFile : Option[File] = None
  
  val panel = SentencePanel.CreatePanel(sentenceFile)
  
	def top = new MainFrame {
	    title = "View simple sentence"
	    minimumSize = new Dimension(400,100)
	    val button = new Button {
	      text = "Vyber soubor"
	      }
	    
	    val label = new Label {
	      text = "No button clicks registered"
	    }	
	    
	    val bp = new BoxPanel(Orientation.Vertical) {
	      contents += SentencePanel.CreateInfoPanel(button,"")
	      contents += panel
	     
	      }
	    
	    contents = bp
	     
	    listenTo(button)
	    
	    reactions += {
	        case ButtonClicked(n)  => {
	          println("Click")
	          sentenceFile = choosePlainFile("Vyber anx soubor")
	          bp.contents.clear
	          bp.contents += { val nameFile = if (sentenceFile.isEmpty) ""
	        		  						  else sentenceFile.get.getName
	          				  
	        	  			  SentencePanel.CreateInfoPanel(button, sentenceFile.get.getName)
	          				 }
	          bp.contents += SentencePanel.CreatePanel(sentenceFile)
	          pack
	          }
	     }
	  }
	
	def choosePlainFile(title: String = ""): Option[File] = {  
    val chooser = new FileChooser(new File("../Result"))
    chooser.title = title
    val result = chooser.showOpenDialog(null)
    if (result == FileChooser.Result.Approve) {
      println("Approve -- " + chooser.selectedFile)
      Some(chooser.selectedFile)
    } else None
  }

}