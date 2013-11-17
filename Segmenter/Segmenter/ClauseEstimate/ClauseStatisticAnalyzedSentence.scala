package ClauseEstimate

import DataObjects.EstimateSentence
import common.sentence.AnxSentence
import common.segment.Segment
import common.segment.InfoSegment
import common.segment.BaseSegment
import common.segment.AnalyzedSegment

class ClauseStatisticAnalyzedSentence(sentence : AnxSentence, path : List[Int] )
	extends  EstimateSentence
{
  
   
  var estimatedSegments = this.applyBestPath(sentence,path)
  
  def applyBestPath(sentence : AnxSentence, path : List[Int]) : List[Segment] = {
   
     def applyBestPath(sentence : List[Segment], path : List[Int], acc : List[Segment]) : List[Segment] = 
     {
       if (sentence.isEmpty)
       {
         acc.reverse
       }
       else
       {       
         val newSegment = sentence.head
         newSegment.clause = path.head
         val segTest = new AnalyzedSegment( newSegment,  newSegment.level.getExactLevel , path.head, false ) 
         segTest
         applyBestPath(sentence.tail, path.tail, segTest :: acc)
       }
     }   
    val newSegments = applyBestPath(sentence.segments, path, List[Segment]())
  newSegments
  }
  
    
  override def toString =  this.estimatedSegments.map(t => t.level.toString + " "+ t.clause +" " + t.getStartNewClause.toString + " \n ").toList.reduce(_ + _)
  
  override val getEstimateSegments = this.estimatedSegments
  val getTestList    = this.getEstimateSegments.zipWithIndex.map(s => (s._2, s._1._clause)).toList
  override val getClause    = this.getEstimateSegments.zipWithIndex.map(s => (s._2, s._1.clause)).toList.groupBy( f => f._2).map(f => (f._1, f._2.map(t => t._1)))
  override val getEstimationOfCountClause : Int = this.countEstimate(this.estimatedSegments.map(segment => BaseSegment.createInfoSegment(segment)).toList, false, false,false, 0)
  override val getCountOfClause :Int = this.getEstimateSegments.map(f => f.clause).toList.max
  
    
   def countEstimate(segments : List[InfoSegment], inBracket : Boolean , countBoundary : Boolean, haveVerb : Boolean , acc : Int) : Int = {
      if (segments.isEmpty){
        
    	  if (haveVerb) {
    	    acc
    	  }
    	  else{
    	    acc + 1
    	  }
      }
      else {
        if (!inBracket && segments.head.HaveOpeningBracket) {
         
           countEstimate(segments.tail,true,countBoundary,haveVerb,acc + 1)
        }
        else if ((!inBracket) && segments.head.HaveCloseBracket) {
         
          countEstimate(segments.tail,false,countBoundary,haveVerb,acc + 1)
        }
        else {
        val new_acc : Int = {
         if (countBoundary && segments.head.IsBoundarySegment ){
           if (segments.tail.isEmpty) {
             acc + 1
           }
           else if (segments.tail.head.IsBoundarySegment){
             acc
           }
           else {
             acc +1
           }
         }
         else if (!segments.head.IsBoundarySegment && segments.head.HaveActiveVerb){
           acc + 1
         }
         else if (segments.tail.isEmpty && segments.head.IsBoundarySegment && !haveVerb) {
           acc + 1
         }
         else {
           acc
         }
       }
   
        if (segments.head.IsBoundarySegment) {
          
          countEstimate(segments.tail,inBracket,countBoundary,haveVerb,new_acc)
        }
        else if (segments.head.HaveActiveVerb) {
          countEstimate(segments.tail,false,true,true,new_acc)
        }
        else {
          countEstimate(segments.tail,false,false,haveVerb,new_acc)
        }
        
       }
      }
     }  
}