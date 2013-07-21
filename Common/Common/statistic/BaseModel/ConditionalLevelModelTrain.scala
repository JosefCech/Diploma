package statistic.BaseModel

trait ConditionalLevelModelTrain extends BaseLevelModel  {

    def learnProbabilities : List[(((Int,String,String),(Int,Int)),((Int,String),(Int,Int)))] = {
   
   val simpleTags = this.getSimpleTags
   
   // pro clauze zmenit tvorbu trojic
  
   val tripples = simpleTags.map(s => {
      var actualLevel = 1
      val levelsOnly = s.map(
          s1 => { val diff = this.getObserve(actualLevel, s1.Level)
                  actualLevel = s1.Level
                  diff
          }
          
      ).toList ::: List(-1)
      val firstFullTags = List("*") ::: s.map(s2 => s2.tag) ::: List("S")
      val secFullTags = List("*","*") ::: s.map(s2 => s2.tag)
      firstFullTags.zip(secFullTags).toList.zip(levelsOnly).toList.filterNot(p => p._1._2 == "S" && p._1._1 == "S")
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