package statistic.BaseModel

trait ClauseModelTrain extends BaseClauseModel {

   def learnProbabilities : List[((Int,String),(Int,Int))] = {
    var double = this.getClauseTags.flatten.map(t => {
	   (t._1,t._2.tag)
   }).groupBy(t => t).map(f => (f._1, f._2.length))
    var single = this.getClauseTags.flatten.map(t => t._2.tag).groupBy(t => t).map(f => (f._1,f._2.length))
    
    val result = double.map(f => {
      
       val simpleCount = single.find(p => p._1 == f._1._2).head._2
       (f._1,(f._2,simpleCount))
    })
    result.toList
  }
}