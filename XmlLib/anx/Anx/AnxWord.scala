package Anx
import common._

class AnxWord(form:String, tag:String, val sep:Boolean) extends MorfWord(form,tag) {
 def IsSeparator : Boolean = { sep }
}