package common

import java.io;

class Word(val form : String ) {
  def IsSeparator : Boolean = false
  
}

class MorfWord( form : String, val tag: String) extends Word(form) 
{
 
}