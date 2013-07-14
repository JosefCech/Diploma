package statistic.BaseModel

trait ConditionalClauseModelTrain  extends BaseClauseModel {

     def learnProbabilities : List[(((Int,String,String),(Int,Int)),((Int,String),(Int,Int)))] = {
   
   val simpleTags = this.getClauseTags
   
   // pro clauze zmenit tvorbu trojic
   val tripples = simpleTags.map(s => {
      val clausesOnly = s.map(s1 => s1._1).toList ::: List(-1)
      val firstFullTags = List("*") ::: s.map(s2 => s2._2.tag) ::: List("S")
      val secFullTags = List("*","*") ::: s.map(s2 => s2._2.tag)
      firstFullTags.zip(secFullTags).toList.zip(clausesOnly).toList.filterNot(p => p._1._2 == "S" && p._1._1 == "S")
      }
   ).flatten
   
    
   
     val groupTripples = tripples.groupBy(p => p).map(p => (p._1,p._2.length))
    
 
     val groupPairs = tripples.groupBy(f => f._1).map(p => (p._1,p._2.length))
     
     val groupPairs2 = tripples.groupBy(f => (f._1._1,f._2)).map(p => (p._1,p._2.length))
     val groupSingles = tripples.groupBy(f => f._1._1).map(p => (p._1,p._2.length))
      
     val trippleWithData = groupTripples.map( f => {
        val doubleData =  groupPairs.find(p => p._1._1 == f._1._1._1 && p._1._2 == f._1._1._2).head
        ((f._1._2,f._1._1._1,f._1._1._2),(f._2,doubleData._2))
     })
    
    
     val pairsWithData = groupPairs2.map( f => {
        val singleData = groupSingles.find(p => p._1 == f._1._1).head
        ((f._1._2,f._1._1),(f._2,singleData._2))
     })
     

     
     trippleWithData.map(f => {
       val pairData = pairsWithData.find(p => p._1._1 == f._1._1 && p._1._2 == f._1._2).head
       (f,pairData)
     }).toList
 }    
}