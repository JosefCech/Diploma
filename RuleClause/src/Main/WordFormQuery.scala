package Main

import common._
object WordFormQuery {
 def openBracket(words : List[Word]) = {
   val openBrackets = "({[";
   words.filter(p => isMatch(p.form,openBrackets.toList.map(t => t.toString)))
 }
 
 def closeBracket(words : List[Word]) : List[Word] = {
    val closeBrackets = "]})"
  words.filter(p => isMatch(p.form,closeBrackets.toList.map(t => t.toString)))
 }
 
 def quotationMark(words : List[Word]) : List[Word] = {
   val quotation = "\"'"
   words.filter(p => isMatch(p.form,quotation.toList.map(t => t.toString))) 
 }
 
 protected def isMatch(form : String, forms : List[String]) : Boolean = {
   forms.contains(form);
 }
}