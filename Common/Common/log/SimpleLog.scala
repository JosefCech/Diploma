package log

trait SimpleLog {
 var log : String = ""
   
 def addToLog(text : Any) = this.appendLog(text)
 
 def appendLog(text : Any) = this.log+= text.toString + "\n"
 
 def getLog : String = this.log
  
}