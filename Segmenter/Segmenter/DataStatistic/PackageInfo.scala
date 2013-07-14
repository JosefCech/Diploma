package DataStatistic

import Anx.AnxReader
import common.Tag

object PackageInfo extends App {
 override def main(args : Array[String]) = 
 {
    def files = common.Directory.ReadAnxFiles(segmenter.Configuration.DataFolder("TestClause")).toList
    val info = files.map(f => 
      {
          val sentence = AnxReader.ReadAnalyzedSentence(f)
          (sentence.getTagsWithClause,sentence.analyzedSentence.clause)
      })
   
   println("Informace o balíèku")
   println("-- Poèet vìt : " + info.length.toString )
   println("-- Poèet segmentù : " + info.map(f => f._1.length).sum.toString )
   println("-- Poèet klauzí : " + info.map(f => f._2.filterKeys(_ != 0).size).sum.toString)
   
   println("-- Maximální poèet segmentù : " + info.maxBy(f => f._1.length)._1.length.toString )
   println("-- Maximální poèet klauzí : " + info.maxBy(f => f._2.toList.length)._2.toList.length.toString )
   println("-- Maximální hloubka : " + info.maxBy(f => f._1.map(s => s._2.Level).max)._1.map(s => s._2.Level).max.toString)
   println("-- Maximální poèet segmentù : " + info.maxBy(f => f._1.length)._1.length.toString )
   println("-- Max Pomer pocet segmentu vs clause : " + info.map(f => (f._2.size.doubleValue/f._1.length,  f._2.size,f._1.length)).max.toString )
   println("--Min Pomer pocet segmentu vs clause : " + info.map(f => (f._2.size.doubleValue/f._1.length,  f._2.size,f._1.length)).min.toString )
   println("-- Pomer pocet segmentu vs clause : " + (info.map(f => f._2.size).sum.doubleValue / info.map(f => f._1.length).sum).toString )
   println("-- Souveti : " + info.filter(f => 
     {
       f._1.groupBy(f => f._1).filter( c => c._2.filter(s => s._2.haveActiveVerb).length > 0).size > 1
     }  
   ).length.toString + "  / " + (info.filter(f =>  f._1.groupBy(f => f._1).filter( c => c._2.filter(s => s._2.haveActiveVerb).length > 0).size > 1).length.doubleValue / info.length).toString)
   val dataGap = info.filter(f => {
     val zero = f._2.find(p =>p._1 == 0)
     if (!zero.isEmpty)
     {
     f._2.filterNot(p => p._1 == 0).filter(p =>
       {
         containsGap(p._2,zero.head._2,p._2.head)
       }).size > 0
       
     }
     else
     {
        f._2.filterNot(p => p._1 == 0).filter(p => containsGap(p._2,List[Int](),p._2.head)).size > 0
     }
   
   }).size
   println("-- Souveti s dirou :"  + dataGap.toString + " / " + dataGap.doubleValue / info.length)
   
   val baseLineSegments = info.map(f => f._1.filter(p => (p._2.haveSubflag && p._2.Level == 1) || ( !p._2.haveSubflag && p._2.Level == 0)).length).sum
   val baseLineClause = info.map(f=> this.baseClauseCreation(f._1,0, 0)).sum
   val baseLineCount = info.filter(f => {
                  f._1.filter(p => (p._2.haveActiveVerb)).length == f._2.filterKeys(p => p != 0).size ||
                  (f._1.filter(p => p._2.haveActiveVerb).length==0 && f._2.filterKeys(p => p != 0).size == 1)
                  
   } ).length
   
   
   val baseLineSegmentsSent = info.filter(f => f._1.filter(p => (p._2.haveSubflag && p._2.Level == 1) || ( !p._2.haveSubflag && p._2.Level == 0)).length == f._1.length).length
   val baseLineClauseSent = info.filter(f => this.baseClauseCreation(f._1,0, 0) == f._1.maxBy(c => c._1)._1).length
   
    println("-- Baseline pro segmenty (subflag => l == 1 jinak l == 0) : " + (baseLineSegments / info.map(f => f._1.length).sum.doubleValue).toString) 
    println("-- Baseline pro clause (boundary => c == 0 jinak c == 1) : " + (baseLineClause / info.map(f => f._1.length).sum.doubleValue).toString) 
   
     println("-- Baseline pro poèet klauzí (bez aktivního slovesa 1) : " + (baseLineCount.doubleValue / info.length).toString) 
    
    println("-- Baseline pro správnì urèené vìty (segment) : " + (baseLineSegmentsSent.doubleValue / info.length).toString) 
    println("-- Baseline pro správnì urèené vìty (clause) : " + (baseLineClauseSent.doubleValue / info.length).toString) 
     
   // println("-- Maximální poèet klauzí : " + info.maxBy(f => f._1)._1.toString )
  // info.foreach(p => println(p._3))
 }
 
 
   def containsGap(data : List[Int], zeroClause : List[Int] , actualhead : Int) : Boolean =
  {
    if (data.isEmpty)
    {
      false
    }
    else if (data.head > actualhead + 1)
    {
      if (zeroClause.contains(actualhead + 1))
      {
         containsGap(data, zeroClause,data.head+1)
      }
      else 
      {
        true
      }
    }  
    else if (data.length == 1)
    {
      false
    } 
    else if (actualhead + 1 == data.tail.head || zeroClause.contains(actualhead + 1))
    {
         containsGap(data.tail, zeroClause,data.head+1)
    }
    else 
    {
      true
    }
  }
   def baseClauseCreation(data : List[(Int,Tag)],actualClause : Int, acc : Int) : Int =
   {
     if (data.isEmpty) 
     {
       acc
     }
     else if (data.head._2.isBoundary && data.head._1 == 0) 
     {
       baseClauseCreation(data.tail,actualClause,acc +1)
     }
     else if (data.head._2.isBoundary )
     {
       baseClauseCreation(data.tail,actualClause,acc)
     }
     else if (data.head._2.haveActiveVerb && data.head._1 == actualClause + 1)
     {
        baseClauseCreation(data.tail,actualClause+1,acc +1)
     }
     else if (data.head._2.haveActiveVerb)
     {
        baseClauseCreation(data.tail,actualClause+1,acc)
     }
     else if (data.head._1 == actualClause)
     {
        baseClauseCreation(data.tail,actualClause,acc +1)
     }
     else 
     {
      baseClauseCreation(data.tail,actualClause+1,acc)  
     }
   }

}