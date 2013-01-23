package Main

import common._

object TagQuery {
 // TODO pøepsat podmínky do XML formátu
  
  def activeVerb(words : List[MorfWord]) : List[MorfWord] = {
    val setTag : List[String] = List[String]("V")
    val except : List[String] = List[String]("Vf","Vm")
    TagMatcher.MatchSetExcept(words,setTag,except)
  }
  
  def cordConjuction(words : List[MorfWord]) : List[MorfWord] = {
      val setTag : List[String] = List[String]("J^")
      TagMatcher.MatchSetExcept(words, setTag, List[String]())
  }
  
  def reflexivePronoun(words : List[MorfWord]) : List[MorfWord] = {
    val setTag : List[String] = List("P6")
    TagMatcher.MatchSetExcept(words, setTag, List[String]())
  }
}