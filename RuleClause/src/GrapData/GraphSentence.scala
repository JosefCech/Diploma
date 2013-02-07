package GrapData


import scala.swing.{MainFrame, SimpleSwingApplication, ScrollPane, Table}

import Main.{Clauser, RuleClauser}
import common.{AnalyzedSegment}

/*
object GraphSentence extends SimpleSwingApplication {

   val files = Clauser2.getFiles("Results")
   val sentences = files.map(t => (t.getName,Anx.AnxReader.ReadSentence(t))).take(20).toList //.filter(p => p.size > 4).toList;
   val cols = sentences.map(s => s._2.size).toList.maxBy(f => f) + 1
   val analyzedSentences = sentences.map(f => (f._1, new RuleClauser(f._2).analyzeSentence.reverse)).toList
   val rows = analyzedSentences.map(s => s._2.map(t => t._1 + 1).toList.max).toList.sum 
   println(rows)
   val table = new Table(rows,cols);
  
   
   def loadTableData() : Table =
   {
     def loadSentenceData(segments : List[(Int,AnalyzedSegment)],zeroRow : Int , col : Int, level : Int ) : Int = {
       if (segments.isEmpty) {
         zeroRow + level
       }
       else {
         val actualLevel = segments.head._1;
         val levels = List[Int]{level ; actualLevel }
         table.update(zeroRow + actualLevel, col , segments.head._2.segment.ToString)
         loadSentenceData(segments.tail,zeroRow,col + 1,levels.max)
       }
     }
     
     def loadTable(sentences : List[(String,List[(Int,AnalyzedSegment)])] , row : Int) : Unit = {
       if (!sentences.isEmpty) {
         val head = sentences.head
         table.update(row, 0, head._1)
         val newRowZero = loadSentenceData(head._2, row, 1, 0) + 1
         loadTable(sentences.tail,newRowZero)
       }
      }
     loadTable(analyzedSentences,0)
     this.table
   }
     def top : MainFrame = new MainFrame {
		title = "Clauser Statistics"
		 contents = new ScrollPane(loadTableData())
		
		} 	
		  
}
*/
