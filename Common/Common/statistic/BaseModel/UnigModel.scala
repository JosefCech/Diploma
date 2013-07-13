package statistic.BaseModel

trait UnigModel {
 def getProbabilities : List[((Int,String),(Int,Int))]
 def getLamba(index : Int) : Double
 def getCountTags :Int
 
  def getProbability(level : Int , tag : String) : Double = {
     val data = this.getProbabilities.find(p => p._1._1 == level && p._1._2 == tag).toList
     if (data.isEmpty) {
       this.countSmoothData
     }
     else {
       this.getLamba(1) * data.head._2._1 / data.head._2._2 + countSmoothData
     }
  }
  
  def countSmoothData : Double = {
    getLamba(0) / this.getCountTags
  }
  
  override def toString : String = this.getProbabilities.map(t => t._1.toString + " \\ " + t._2.toString ).toList.mkString("\n")
  
  lazy val getProbMax : Double = this.getProbabilities.map(f => this.getProbability(f._1._1, f._1._2)).max
  lazy val getProbMin : Double = this.getProbabilities.map(f => this.getProbability(f._1._1, f._1._2)).min
}