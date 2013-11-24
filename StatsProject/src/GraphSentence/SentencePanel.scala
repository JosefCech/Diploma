package GraphSentence

import scala.swing._
import common.sentence.AnxSentence
import java.awt.Dimension
import java.awt.Color
import java.io.File
import Anx.AnxReader
import java.awt.Font
import scala.swing.GridPanel
import javax.swing.border.EmptyBorder


object SentencePanel {
  def CreatePanel(sentenceFile : Option[File]) : Panel = {
  if (sentenceFile.isEmpty) new GridPanel(1,1) {
   contents += new Label("Prosím vyberte soubor")    
  }
  else {
	  val sentence = AnxReader.ReadAnalyzedSentence(sentenceFile.get)
	  val clause = sentence.segments.zipWithIndex.groupBy(f => f._1.clause).map(t => { 
	   	  (t._2.map(t => t._2),  t._1.toString + ". :" +  t._2.map(s => s._1.toSimpleString).mkString(" "))
	  }
	  )
	  val segments = sentence.segments.zipWithIndex.map( p => { val wholeClause = clause.filter( c => c._1.contains(p._2)).head._2 
		  													if (p._1.level.getExactLevel == -1)
		  													(0, p._1.clause, p._1.toSimpleString, wholeClause )
		  										else (p._1.level.getExactLevel, p._1.clause, p._1.toSimpleString, wholeClause)
	  											}
		  									).toList  
	 
	  val countSegment = segments.size
	  val deepLevel = segments.map(p => p._1).max + 1
	  val panel = new GridBagPanel()
	  {
	    val gc = new Constraints
	    gc.weightx = 0.3
	    gc.weighty = 0.3
	    
	    1 to deepLevel*countSegment  map { 
	                               case i   => {
	                                  val updated = i -1
	                                  val currentLevelGuess = updated / countSegment
	                                  val currentSegmentNum = (updated % countSegment) 
	                                  println(currentSegmentNum.toString + "/" + currentLevelGuess.toString + "/" + segments.size.toString)
	                                  gc.gridy = currentLevelGuess
	                                  gc.gridx = currentSegmentNum
	                                  if (segments.apply(currentSegmentNum)._1 == currentLevelGuess)
	                                  {
	                                    val label = new Label(segments.apply(currentSegmentNum)._3) {
	                                    val preferedWidth : Int = segments.apply(currentSegmentNum)._3.length * 7
	                                    preferredSize_=(new Dimension(preferedWidth,20))
	                                     font_=( new Font("Arial", 0, 10) )                                  
	                                    foreground_=(GetColor(segments.apply(currentSegmentNum)._2))
	                                    tooltip = segments.apply(currentSegmentNum)._4
	                                      
	                                     
	                                    }
	                                    layout(label) = gc
	                                  }
	                                  else {
	                                     val label = new Label()
	                                     {
	                                         preferredSize_=(new Dimension(1,20))
	                                     }
	                                     layout(label) = gc
	                              }
	             }
	        }
	  }
	   panel
  	}
  }
  def GetColor(clause : Int): Color= clause % 12 match {
	    case 1  => Color.RED
	    case 2 => Color.CYAN
	    case 3 => Color.BLUE
	    case 4 => Color.PINK
	    case 5 => Color.DARK_GRAY
	    case 6 => Color.GREEN
	    case 7 => Color.MAGENTA
	    case 8 => Color.ORANGE
	    case 9 => Color.YELLOW
	    case 10 => new Color(131, 111, 255) // slate blue
	    case 11 => new Color(192, 255, 62) // olive drab
	    case _  => Color.BLACK
	   
	 }
  def CreateInfoPanel(button : Button, lText : String ) : Panel = {
    val panel = new BoxPanel(Orientation.Horizontal) {
    	contents += new Label(lText)
    	contents += button
    	
    	border = new EmptyBorder(10,10,10,10)
    }
    panel
  }
}