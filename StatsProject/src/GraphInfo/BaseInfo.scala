package GraphInfo

import Stats._
import scala.swing._
import scala.swing.event._

object BaseInfo extends SimpleSwingApplication {
  

  def top = new MainFrame {
		title = "Clauser Statistics"
		contents = new ScrollPane(new BaseInfoContainer("GoldenData").box)
  	}
  
	
}