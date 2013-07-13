package statistic.BaseModel

trait ConditionalModel {
  
 def getProbabilities : List[(((Int,String,String),(Int,Int)),((Int,String),(Int,Int)))]
 def getLamba(index : Int) : Double
 def getCountTags :Int
 
   def getProbability(level : Int , tag1 : String, tag2  : String ) : Double = {
     val dataTripple = this.getProbabilities.find(p => p._1._1._1 == level && p._1._1._2 == tag1 && p._1._1._3 == tag2)
     val dataPair = this.getProbabilities.find(p => p._2._1._1 == level && p._2._1._2 == tag1)
     var trippleProb : Double = 0
     var pairProb : Double = 0
     if (!dataTripple.isEmpty)
     {
      trippleProb = this.getLamba(2) * dataTripple.head._1._2._1 / dataTripple.head._1._2._2
     }
     
     if (!dataPair.isEmpty)
     {
       pairProb = this.getLamba(1)  * dataPair.head._2._2._1 / dataPair.head._2._2._2
     }
      trippleProb + pairProb + this.countSmoothData
  }
  
  def countSmoothData : Double = {
    this.getLamba(0) / this.getCountTags
  }
 
 override def toString = this.getProbabilities.map( t => t.toString).mkString("\n")

}