package statistic.BaseModel

trait LevelModelTrain extends BaseLevelModel {

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
   
  
}