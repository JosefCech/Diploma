package Rules
/** Sentence from list of word. This class is inherited from 
 * common.Sentence  
 * Extra functions : 
 * 		
 * 
 * @constructor create sentence from words and ident (not required)
 * @param sentence all words of sentence (base words with morphologic information)
 * @param ident identificator  
 */
import common.{MorfWord,Word,AnalyzedSegment,Segment}

class AnalyzedSentence(sentence : List[Segment],val ident : String)  {

    /** List segment with bonus properties on morphological information */
   def morfSegments : List[AnalyzedSegment] =  sentence.map(t => new AnalyzedSegment(t)).toList 
   
    /** List segment with bonus properties on morphological information - indexed */
   def morfSegmentsIndexed : List[(Int,AnalyzedSegment)] =  sentence.map(t => new AnalyzedSegment(t)).toList.zipWithIndex.map(s => s.swap).toList 
   
   private var tested : Boolean = false
   
   /** Found pairs boundary */
   private def containsPairBounary(segments : List[AnalyzedSegment]) : Boolean = 
    this.morfSegments.filter(f => f.HaveOpeningBracket.size > 0 || f.HaveQuotationMark.size > 0).size > 0
  
    /** Get group of segments and its level */
   private def parsedAccordingPairBoundary(segments : List[AnalyzedSegment]) : Array[(Int,List[AnalyzedSegment])] = {
     
   def parsingData(segments : List[AnalyzedSegment],
    				level: Int,
    				countDash : Int,
    				pairDash : Boolean,
    				queue : List[String],
    				accList : List[AnalyzedSegment],
    				accArray : Array[(Int,List[AnalyzedSegment])]
    ) 
    : Array[(Int,List[AnalyzedSegment])]  = {
      if (segments.isEmpty) {
        if (accList.isEmpty){
          accArray
        }
        else if (!queue.isEmpty) { 
          throw new Exception("Parsing Error")
        }
        else {
          accArray ++ Array((0,accList))
        }
      }
      else {
        val analyzedSegment = segments.head
        if (analyzedSegment.HaveCloseBracket.size > 0) {
           val foundBracket = analyzedSegment.HaveCloseBracket.head
           val requiredBracked = queue.head
           if (!foundBracket.equals(requiredBracked)){
             queue.foreach(f => println(f.toString))
             println("head " + queue.head)
             throw new Exception("Parsing Error - No required bracket  " + requiredBracked + " " + foundBracket.form + " " + ident)
           }         
           parsingData(segments.tail, level-1,countDash,pairDash,queue.tail,List(analyzedSegment),accArray ++ Array((level,accList)))
        }
        else if (analyzedSegment.HaveOpeningBracket.size > 0) {
             val foundBracket = analyzedSegment.HaveOpeningBracket.head
             val requiredBracket = foundBracket.syntacticOpposite
             parsingData(segments.tail, level + 1,countDash,pairDash, requiredBracket :: queue, List[AnalyzedSegment](),accArray ++ Array((level,accList ::: List(analyzedSegment))))
        }
        else if (analyzedSegment.HaveDash && countDash > 0 && pairDash){
          if ((countDash % 2) > 0) {
             parsingData(segments.tail, level-1,countDash-1,pairDash,queue.tail,List(analyzedSegment),accArray ++ Array((level,accList)))
          }
          else{
             parsingData(segments.tail, level + 1,countDash-1,pairDash, "-" :: queue,List[AnalyzedSegment](),accArray ++ Array((level,accList ::: List(analyzedSegment))))
          }
        }
        else {
          
        parsingData(segments.tail, level, countDash,pairDash,queue,List[AnalyzedSegment](),accArray ++ Array((level,accList ::: List(analyzedSegment))))
        }
        
      }
    }
   var dashPair = segments.count(p => p.HaveOpeningBracket.size > 0) == 0 && segments.count(p => p.HaveDash) == 2
   this.tested = dashPair
   println(this.ident + " " +dashPair)
   parsingData(segments,0,segments.count(p => p.HaveDash),dashPair,List[String](),List[AnalyzedSegment](),Array[(Int,List[AnalyzedSegment])]())
  }
  
    /** Using pair of boundary and subflags to setting each segment its level */
  val analyzedSentence : List[(Int,AnalyzedSegment)] = {
    
     val firstStep = { if ((this.containsPairBounary(this.morfSegments))) {
			           this.parsedAccordingPairBoundary(this.morfSegments)
			         }
     				else {
     				   Array((0,this.morfSegments))
     				}

     }
     
    def useSubflags(pseudoClause : Array[(Int,List[AnalyzedSegment])], segments : List[AnalyzedSegment], level : Int, previousCoord : Boolean,  acc : List[(Int,AnalyzedSegment)]) : List[(Int,AnalyzedSegment)]
    = {
        if (segments.isEmpty)
        {
          if (pseudoClause.isEmpty) { 
            acc
          }
          else { 
            useSubflags(pseudoClause.tail,pseudoClause.head._2,pseudoClause.head._1,false,acc)
          }
        } 
        else {
          val actualSegment = segments.head
          val nextSegment = { 
            if (segments.tail.isEmpty) {
              null
            } 
            else {
             segments.tail.head  
            }
          }
          val previousLevel = {
             if (acc.isEmpty) { 
               0
             }
             else {
               acc.head._1
             }
          }
          
          if (actualSegment.IsBoundarySegment  && 
        	  actualSegment.segment.words.head == ".") {
            useSubflags(pseudoClause,segments.tail,List(level-1,0).max,false,(0,actualSegment) :: acc)
          }
          else if (actualSegment.HaveSubFlag){
            if (previousLevel == level + 1){
              useSubflags(pseudoClause,segments.tail,acc.head._1,false,(previousLevel + 1,actualSegment) :: acc)
            }
            else {
                useSubflags(pseudoClause,segments.tail,level,false,(level + 1,actualSegment) :: acc)
            }
          }
          else {
             if (previousLevel == level + 1) { 
              
               if  (actualSegment.HaveCordConjuction){
              // this.tested = true; 
            	 useSubflags(pseudoClause,segments.tail,level,true,(previousLevel,actualSegment) :: acc)
               }
               else if (previousCoord)  {
                
                 // println(this.ident + "/" +previousLevel)
                 // println(actualSegment.segment)
                useSubflags(pseudoClause,segments.tail,level,false,(previousLevel,actualSegment) :: acc)
                //useSubflags(pseudoClause,segments.tail,List(level-1,0).max,false,(level,actualSegment) :: acc)
                  
               }
               else if (nextSegment != null && 
            		   actualSegment.IsBoundarySegment && 
            		   actualSegment.segment.words.head.form == "," &&
            		   nextSegment.HaveSubFlag &&
            		   !nextSegment.IsBoundarySegment
               ) {
                  useSubflags(pseudoClause,segments.tail,level,false,(previousLevel,actualSegment) :: acc)
               }
               else {
                  useSubflags(pseudoClause,segments.tail,List(level-1,0).max,false,(level,actualSegment) :: acc)
               }
               
               }
              else {
               	useSubflags(pseudoClause,segments.tail,level,false,(level,actualSegment) :: acc)
               }
               
          }
          
        }
       }
      
    
        
       val secondStep = useSubflags(firstStep,List[AnalyzedSegment](),0,false,List[(Int,AnalyzedSegment)]()) 
       val data = secondStep
   
       data
    }
  
  val levelConfiguration  = {
     analyzedSentence.map(t => t._1).toList.reverse.foldLeft("")((r,a) => r + a.toString)
  }
  
  def isForTesting : Boolean = this.tested;
  
 def getLevels : List[Int] = this.analyzedSentence.map(t => t._1).toList.reverse
}

