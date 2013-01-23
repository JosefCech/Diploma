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

   def parsedSegments = {
      def segments(words : List[MorfWord] , acc : List[Segment] , accWord : List[MorfWord]) : List[Segment] = {
        if (words.isEmpty) { 
          if (!accWord.isEmpty)
          {
           if (accWord.head.isSeparator) 
             (new Boundary(accWord.reverse,-1):: acc).reverse
           else {
             (new PureSegment(accWord.reverse,-1) :: acc).reverse
           }
          }
          else {
            acc.reverse
          }
        }
        else if (words.head.isSeparator)
         {
          if (accWord.isEmpty) {
           segments(words.tail,(new Boundary(List(words.head),-1):: acc),accWord)
          }
          else {
             segments(words.tail,(new Boundary(List(words.head),-1) :: (new PureSegment(accWord.reverse,-1) :: acc)),List[MorfWord]())
          }
        }
        else {
           segments(words.tail,acc,words.head :: accWord)
        }
      }
     segments(morfSentence,List[Segment](),List[MorfWord]())
   }
   }
