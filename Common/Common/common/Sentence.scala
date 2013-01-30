package common

class Sentence(val sentence : List[Word], val ident : String = "" ) {

   def morfSentence = sentence.map(t => t match {
        						case t : MorfWord =>  t 
        						case t : Word => new MorfWord(t.form,"")   
        						case _ => throw new ClassCastException
                                } ).filter(_.tag != "").toList
   def MorfWords = sentence.map(t => t match {
        						case t : MorfWord =>  t 
        						case t : Word => new MorfWord(t.form,"")   
        						case _ => throw new ClassCastException
                                } ).filter(_.tag != "").toList
                       
   def GetBounderies = {
	   					morfSentence.filter(t=> t.isSeparator).toList
   						}
                      
   def GetSubFlags = {
	   				   morfSentence.filter( t => t.isSubFlag ).toList	   					  
	   				  }
   
   def Ident = this.ident
   
   def ToString =   sentence.map(s => s match {
    case s : Word => s.form
  	}
   ).mkString(" ") 

   def parsedSegments : List[Segment] = this.parsedSegments(List[(Int,Int)]())
   def parsedSegments(levels : List[(Int,Int)]) : List[Segment] = {
      def segments(words : List[MorfWord] , acc : List[Segment] , accWord : List[MorfWord], levels : List[(Int,Int)]) : List[Segment] = {
        var restLevels = List[(Int,Int)]();
        var	originLevels =  List[(Int,Int)]();
        var level = -1;
        if (!levels.isEmpty)
        {
          restLevels = levels.tail
          originLevels = levels
          level = levels.head._1
        }
        if (words.isEmpty) { 
          if (!accWord.isEmpty)
          {
           if (accWord.head.isSeparator) 
             (new Boundary(accWord.reverse,level):: acc).reverse
           else {
             (new PureSegment(accWord.reverse,level) :: acc).reverse
           }
          }
          else {
            acc.reverse
          }
        }
        else if (words.head.isSeparator)
         {
          if (accWord.isEmpty) {
           segments(words.tail,(new Boundary(List(words.head),level):: acc),accWord, restLevels)
          }
          else {
            var nextLevel =  -1 
            if (!restLevels.isEmpty){
            nextLevel = restLevels.head._1
            restLevels = restLevels.tail
            }
             segments(words.tail,(new Boundary(List(words.head),nextLevel) :: (new PureSegment(accWord.reverse,level) :: acc)),List[MorfWord](),restLevels)
          }
        }
        else {
           segments(words.tail,acc,words.head :: accWord , originLevels)
        }
      }
     segments(morfSentence,List[Segment](),List[MorfWord](), levels)
   }
   
 def segments = this.parsedSegments(List[(Int,Int)]())
   }
