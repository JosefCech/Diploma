package DataStatistic

import Anx.AnxReader
import common.Tag
import common.sentence.AnalyzedSentence

object PackageInfo extends App {
 override def main(args : Array[String]) = 
 {
    def files = common.Directory.ReadAnxFiles(segmenter.Configuration.DataFolder("Test")).toList
    val info = files.map(f => 
      {
          val sentence = AnxReader.ReadAnalyzedSentence(f)
          val dashData = sentence.sentenceWithData.zipWithIndex.filter(p => p._1.words.filter(p => p.equals("-")).length > 0)
         
          val countData = dashData.filter(f => f._2 != 0).map(f => {
            val index = f._2
            
            val data = sentence.sentenceWithData.zipWithIndex
            
            val prevSeq = data.filter(p => p._2 < index && !p._1.isBoundary )
            val nextSeq = data.filter(p => p._2 > index && !p._1.isBoundary )
            if (prevSeq.isEmpty)
            {
             
                (false,(0,0),(0,0))
            }
            else if (nextSeq.isEmpty)
            {
             
               (false,(0,0),(0,0))
            }
            else
            {
           
            val prev = prevSeq.maxBy(_._2)
            val next = nextSeq.minBy(_._2)
            (true,(prev._1.Level,next._1.Level),(prev._1.ClauseNum,next._1.ClauseNum))
            }
          })
          (sentence.getTagsWithClause,sentence.analyzedSentence.clause, countData)
      })
   
   println("Okol� poml�ky - level")
   val infoGroup = info.map(f => f._3).flatten
   val levelInfo = infoGroup.filter(f => f._1).map(f => f._2._1 - f._2._2).groupBy(f => f).map(f => (f._1, f._2.length)).toList.sortBy(f => f._2).reverse
   println(levelInfo)
   println("Okol� poml�ky - clause")
   val clauseInfo = infoGroup.filter(f => f._1).map(f => f._3._1 - f._3._2).groupBy(f => f).map(f => (f._1, f._2.length)).toList.sortBy(f => f._2).reverse
   println(clauseInfo)
    
   println("Informace o bal��ku")
   println("-- Po�et v�t : " + info.length.toString )
   println("-- Po�et segment� : " + info.map(f => f._1.length).sum.toString )
   println("-- Po�et klauz� : " + info.map(f => f._2.filterKeys(_ != 0).size).sum.toString)
   
   println("-- Maxim�ln� po�et segment� : " + info.maxBy(f => f._1.length)._1.length.toString )
   println("-- Maxim�ln� po�et klauz� : " + info.maxBy(f => f._2.toList.length)._2.toList.length.toString )
   println("-- Maxim�ln� hloubka : " + info.maxBy(f => f._1.map(s => s._2.Level).max)._1.map(s => s._2.Level).max.toString)
   println("-- Maxim�ln� po�et segment� : " + info.maxBy(f => f._1.length)._1.length.toString )
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
   
     println("-- Baseline pro po�et klauz� (bez aktivn�ho slovesa 1) : " + (baseLineCount.doubleValue / info.length).toString) 
    
    println("-- Baseline pro spr�vn� ur�en� v�ty (segment) : " + (baseLineSegmentsSent.doubleValue / info.length).toString) 
    println("-- Baseline pro spr�vn� ur�en� v�ty (clause) : " + (baseLineClauseSent.doubleValue / info.length).toString) 
     
   // println("-- Maxim�ln� po�et klauz� : " + info.maxBy(f => f._1)._1.toString )
  // info.foreach(p => println(p._3))
 }
   def isComplexSentence(sentence : AnalyzedSentence) : Int = {
    val data : List[(Int,Tag)] = sentence.segments.map( segment => {
      (segment.clause, segment.taggedSegment.GetTag )  
    })
    data.groupBy(f => f._1).filter( c => c._2.filter(s => s._2.haveActiveVerb).length > 0).size 
   }
 
   def containsGap(sentence : AnalyzedSentence) : Boolean =
   {
     val zero = sentence.clause.find(p =>p._1 == 0)
     if (!zero.isEmpty)
     {
     sentence.clause.filterNot(p => p._1 == 0).filter(p =>
       {
         containsGap(p._2,zero.head._2,p._2.head)
       }).size > 0
       
     }
     else
     {
        sentence.clause.filterNot(p => p._1 == 0).filter(p => containsGap(p._2,List[Int](),p._2.head)).size > 0
     }
   
   }
   
   def containsGap(data : List[Int], zeroClause : List[Int] , actualhead : Int) : Boolean = {
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
   def baseClauseCreation(data : List[(Int,Tag)],actualClause : Int, acc : Int) : Int = {
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