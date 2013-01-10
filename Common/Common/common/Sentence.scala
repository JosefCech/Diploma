package common

class Sentence(sentence : List[Word] ) {

   protected def morfSentence = sentence.map(t => t match {
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
   def ToStrng =   sentence.map(s => s match {
    case s : Word => s.form
  	}
   ).mkString(" ") 

   def parsedSegments = {
      def segments(words : List[MorfWord] , acc : List[Segment] , accWord : List[MorfWord]) : List[Segment] = {
        if (words.isEmpty) (new PureSegment(accWord.reverse) :: acc).reverse
        else if (words.head.isSeparator && !accWord.isEmpty) {
          if (accWord.head.isSeparator)
          {
           segments(words.tail,acc,words.head :: accWord)
          }
          else {
            segments(words.tail,(new PureSegment(accWord.reverse) :: acc),List(words.head))
          }
        }
        else {
           segments(words.tail,acc,words.head :: accWord)
        }
      }
     segments(morfSentence,List[Segment](),List[MorfWord]())
   }
   }
