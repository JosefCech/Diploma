package common

import java.io;

class Word(val form : String) {
  def IsSF : Boolean = false
  def IsDelimiter : Boolean = false
  
}

class MorfWord( form : String, val tag: String) extends Word(form) 
{
 
}