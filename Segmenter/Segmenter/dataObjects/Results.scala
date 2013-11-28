package DataObjects

import Xml.XmlWritable
import java.io.PrintWriter
import Translation.Translation
import java.io.File
import scala.xml.Node
import common.sentence.AnalyzedSentence


   //(stejný počet segmenů, počet správně určených segmentů , počet  špatně určených segmentů , rozštěpení klauze, počet vět)
	

class ResultsLevel(val results : List[(Boolean, Int, Int, Boolean, Int )]) extends XmlWritable {
	
    val wholeCount = results.map( a => a._2 + a._3).toList.sum
    val rightCount = results.map(a => a._2).toList.sum
    val wrongCount = results.map(a => a._3).toList.sum
    val rightCountSentence : Int= results.filter(p => p._3 == 0).length 
    val wholeCountSentence : Int = results.length
	    
	 override val TransformXml : Node =  <results>
 											<segments right={(rightCount.doubleValue/wholeCount).toString} wholeCount={wholeCount.toString} rightCount={rightCount.toString}  />
 											<sentence right={(rightCountSentence.doubleValue/wholeCountSentence).toString} wholeCount={wholeCountSentence.toString} rightCount={rightCountSentence.toString} />
 										 </results>
		 
	
	 def printGroupData(data : List[ (Boolean,Int,Int , Boolean,Int)], headline : String , prefix : String) : Unit = 
	 {
	   println(createDataText(data, headline, prefix))
	 }
 
	 def printGroupDataIntoFile(pw : PrintWriter, data : List[ (Boolean,Int,Int , Boolean,Int)], headline : String , prefix : String) : Unit = {
		 pw.write(createDataText(data, headline, prefix))
	 }
 
	 def createDataText(results : List[ (Boolean,Int,Int , Boolean,Int)], headline : String , prefix : String) : String = 
	 {
	    val wholeCount = results.map( a => a._2 + a._3).toList.sum
	    val rightCount = results.map(a => a._2).toList.sum
	    val wrongCount = results.map(a => a._3).toList.sum
	    val rightCountSentence = results.filter(p => p._3 == 0).length 
	    
	    var builder = new StringBuffer
	    
	    builder.append(prefix +(headline + "-------------------------------------------------").take(50))
	    builder.append("\n")
	    builder.append(prefix +"Right Segments:")
	    builder.append("\n")
	    builder.append(prefix + (rightCount.doubleValue/wholeCount).toString )
	  
	    builder.append(prefix + rightCount.toString + " " + wholeCount.toString)
	    builder.append("\n")
	    builder.append(prefix + "Wrong Segments:")
	    builder.append("\n")
	    builder.append(prefix + wrongCount.doubleValue/wholeCount )
	    
	    builder.append(prefix + wrongCount.toString + " " + wholeCount.toString)
	    builder.append("\n")
	    builder.append(prefix + "Right Sentence:")
	    builder.append("\n")
	    builder.append(prefix + rightCountSentence.doubleValue/ results.length )
	    builder.append("\n")
	    builder.append(prefix + "Wrong Sentence:")
	    builder.append("\n")
	    builder.append(prefix + ((results.length - rightCountSentence).doubleValue/ results.length).toString)
	    builder.append("\n")
	    builder.append(prefix + results.length.toString)
	    builder.append("\n")
	    builder.append(prefix + ("end "+ headline + "---------------------------------------------").take(50))
	    builder.toString
	 }

	 def print(intoFile : Boolean , headLine : String, dataSet : String) : Unit = {
		 if (intoFile) {
			    println((headLine + "-------------------------------------------------------------------------------------------------").take(60))
			    printGroupData(results,Translation.succesTotal, "")
			    printGroupData(results.filter(f => f._5 > 1).toList,Translation.succesComplex, "\t")
			    
			    results.groupBy(f => f._5).filterNot(f => f._1 < 2).map(f => f._1).toList.sorted.foreach(s => 
			       printGroupData(results.filter(f => f._5 == s).toList,Translation.succesComplexWith(s), "\t\t") 
			    )
			    printGroupData(results.filter(f => f._4).toList,Translation.succesSplitSentence, "\t")
			    println(("end " +headLine + "-------------------------------------------------------------------------------------------------").take(60))
			} else {
			     val rw = new java.io.PrintWriter(new File("result" + dataSet))
			     rw.print((headLine + "-------------------------------------------------------------------------------------------------").take(60))
			     printGroupDataIntoFile(rw,results,Translation.succesTotal, "")
			     printGroupDataIntoFile(rw,results.filter(f => f._5 > 1).toList,Translation.succesComplex, "\t")
			    
			     results.groupBy(f => f._5).filterNot(f => f._1 < 2).map(f => f._1).toList.sorted.foreach(s => 
			     printGroupDataIntoFile(rw,results.filter(f => f._5 == s).toList,Translation.succesComplexWith(s), "\t\t") 
			     )
			    printGroupDataIntoFile(rw,results.filter(f => f._4).toList,Translation.succesSplitSentence, "\t")
			     rw.print(("end " +headLine + "-------------------------------------------------------------------------------------------------").take(60))
			    }
	 		}
	 } 

class ResultsClause(val results : List[(Int, Int, AnalyzedSentence, (Int, Int, Int), Boolean, Int )]) extends XmlWritable {
	
	val wholeCount = results.map( a => a._1 + a._2).toList.sum
    val rightCount = results.map(a => a._1).toList.sum
    val wrongCount = results.map(a => a._2).toList.sum
   
    val rightCountSentence = results.filter(p => p._2 == 0).length 
    val wholeCountSentence = results.length
    
    val clauseData = results.map(f => (f._4._1,f._4._2,f._4._3,f._5)).toList
	
	 override val TransformXml : Node =  <results>
 											<segments right={(rightCount.doubleValue/wholeCount).toString} wholeCount={wholeCount.toString} rightCount={rightCount.toString}  />
 											<clause right={(clauseData.map(t => t._1).toList.sum.doubleValue / clauseData.map(t => t._1 + t._2 + t._3).sum).toString} rightCount={clauseData.map(t => t._1).toList.sum.toString} wholeCount={clauseData.map(t => t._1 + t._2 + t._3).sum.toString} />
 											<sentence right={(rightCountSentence.doubleValue/wholeCountSentence).toString} wholeCount={wholeCountSentence.toString} rightCount={rightCountSentence.toString} />
 										 </results>
  def createResultData(data : List[(Int,Int , AnalyzedSentence, (Int,Int,Int), Boolean,Int)], headLine : String , prefix : String) : String =
  {
    val wholeCount = data.map( a => a._1 + a._2).toList.sum
    val rightCount = data.map(a => a._1).toList.sum
    val wrongCount = data.map(a => a._2).toList.sum
   
    val rightCountSentence = data.filter(p => p._2 == 0).length 
    
     val clauseData = data.map(f => (f._4._1,f._4._2,f._4._3,f._5)).toList
    
    var builder = new StringBuilder
    builder.append(prefix + (headLine + "-------------------------------------------------------------------------------------------------").take(60))
    builder.append("\n")
    builder.append(prefix + "Right Segments:")
    builder.append("\n")
    builder.append(prefix + rightCount.doubleValue/wholeCount )
    builder.append("\n")
    builder.append(prefix + rightCount.toString + " " + wholeCount.toString)
    builder.append("\n")
    builder.append(prefix + "Wrong Segments:")
    builder.append("\n")
    builder.append(prefix + wrongCount.doubleValue/wholeCount )
    builder.append("\n")
    builder.append(prefix + wrongCount.toString + " " + wholeCount.toString)
    builder.append("\n")
    builder.append(prefix + "Right selected clause : ")
    builder.append("\n")
    builder.append(prefix + clauseData.map(t => t._1).toList.sum.doubleValue / clauseData.map(t => t._1 + t._2 + t._3).sum)
    builder.append("\n")
    builder.append(prefix + "Wrong 1 - k existujicim klauzim segmenty navic")
    builder.append("\n")
    builder.append(prefix + clauseData.map(t => t._2).toList.sum.doubleValue / clauseData.map(t => t._1 + t._2 + t._3).sum)
    builder.append("\n")
    builder.append(prefix + "Wrong 2 - spatny pocet klauzi")
    builder.append("\n")
    builder.append(prefix + clauseData.map(t => t._3).toList.sum.doubleValue / clauseData.map(t => t._1 + t._2 + t._3).sum)
    builder.append("\n")
    builder.append(prefix + "Right Sentence:")
    builder.append("\n")
    builder.append(prefix + rightCountSentence.doubleValue/ data.length )
    builder.append("\n")
    builder.append(prefix + "Wrong Sentence:")
    builder.append("\n")
    builder.append(prefix + (data.length - rightCountSentence).doubleValue/ data.length )
    builder.append("\n")
    builder.append(prefix + data.length.toString)
    builder.append("\n")
     builder.append(prefix +("end" + headLine + "-------------------------------------------------------------------------------------------------").take(60))
  
    builder.toString
  }
  def print( notGoodAnalyzed : Int, notGoodSimple : Int) : Unit =
  {
  	 //print(Translation.wrongTagged)
     println( 	"( " + (notGoodAnalyzed.doubleValue / results.length).toString
    		 	+ "," + (notGoodSimple.doubleValue / results.length).toString + " )" 
    		 )
    		 
    println("")
    println(this.createResultData(results, "Level annotated data - whole data", ""))
    println(this.createResultData(results.filter(p => p._5), "Level annotated data - with break", "\t"))
    println(this.createResultData(results.filter(p => p._6 > 1), "Level annotated data -complex sentence", "\t"))
  }
}