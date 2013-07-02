package common.sentence

import common.segment.{ Segment, Boundary, PureSegment}
import common.MorfWord

trait SegmentedSentence {
 /** List of tuple (Level,Segment) where Level has  value -1 from external source */
   def parsedSegments(words: List[MorfWord], levels : List[(Int,Int)]) : List[Segment] = {
     
    def segments(words : List[MorfWord] , acc : List[Segment] , accWord : List[MorfWord], levels : List[(Int,Int)]) : List[Segment] = {
        // zbytkove levely
        var restLevels = List[(Int,Int)]();
        // puvodni levely
        var	originLevels =  List[(Int,Int)]();
        // aktualni level
        var level = -1;
        
        // pokud jsou urovne neprazdne dopln promene
        if (!levels.isEmpty)
        {
          restLevels = levels.tail
          originLevels = levels
          level = levels.head._1
        }
        
        if (words.isEmpty) {
          // nemam dalsi slova
          if (!accWord.isEmpty)
          {
            // ale mam neco v word kontejneru - vytvorim segment a pridam mu prislusny level
           if (accWord.head.isSeparator)  // pokud je to delimiter 
             (new Boundary(accWord.reverse,level):: acc).reverse
           else { // pokud to neni oddelovac
             (new PureSegment(accWord.reverse,level) :: acc).reverse
           }
          }
          else {
             // nemam nic ve word kontejneru tak vratim reverse segment kontejner
            acc.reverse
          }
        }
        else if (words.head.isSeparator)
        { // pokud dalsi slovo je oddelovac
          if (accWord.isEmpty) { // je prazdny word kontejner
            // zalozim novy boundary segment
           segments(words.tail,(new Boundary(List(words.head),level):: acc),accWord, restLevels)
          }
          else {
            // pokud mam neco ve word kontejneru musim zalozit puresegment a boundary segment
            var nextLevel =  -1 
            if (!restLevels.isEmpty){
            nextLevel = restLevels.head._1
            restLevels = restLevels.tail
            }
             segments(words.tail,(new Boundary(List(words.head),nextLevel) :: (new PureSegment(accWord.reverse,level) :: acc)),List[MorfWord](),restLevels)
          }
        }
        else {
           // pridam nove slovo do word kontejneru
           segments(words.tail,acc,words.head :: accWord , originLevels)
        }
      }
    
     segments(words,List[Segment](),List[MorfWord](), levels)
   }
  
}