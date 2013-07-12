package statistic

trait LevelSegmentModel extends LevelModel {

  lazy val probabilities = this.learnProbabilities
  
 lazy val countTags : Int = (this.getSimpleTags.flatten.groupBy(f => f.simpleTag).toList.length + 1) * 10  
 
 var lamba0 : Double = (1).doubleValue / 9
 var lamba1 : Double = 1 - lamba0
 
  def learnProbabilities : List[((Int,String),(Int,Int))] = {
    var double = this.getSimpleTags.flatten.map(t => {
	   (t.Level,t.simpleTag)
   }).groupBy(t => t).map(f => (f._1, f._2.length))
    var single = this.getSimpleTags.flatten.map(t => t.simpleTag).groupBy(t => t).map(f => (f._1,f._2.length))
    
    val result = double.map(f => {
      
       val simpleCount = single.find(p => p._1 == f._1._2).head._2
       (f._1,(f._2,simpleCount))
    })
    result.toList
  }
  
  def getProbability(level : Int , tag : String) : Double = {
     val data = probabilities.find(p => p._1._1 == level && p._1._2 == tag).toList
     if (data.isEmpty) {
       this.countSmoothData
     }
     else {
       lamba1 * data.head._2._1 / data.head._2._2 + countSmoothData
     }
  }
  
  def countSmoothData : Double = {
    lamba0 / countTags
  }
  
  override def toString : String = this.probabilities.map(t => t._1.toString + " \\ " + t._2.toString ).toList.mkString("\n")
  
  lazy val getProbMax : Double = this.probabilities.map(f => this.getProbability(f._1._1, f._1._2)).max
  lazy val getProbMin : Double = this.probabilities.map(f => this.getProbability(f._1._1, f._1._2)).min
}