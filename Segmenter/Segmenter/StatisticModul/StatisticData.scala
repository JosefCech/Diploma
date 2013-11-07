package StatisticModul

import Anx.AnxReader
import common.segment.BaseSegment
import java.io.File

object StatisticData extends App {

   override def main(args : Array[String]) = {
     def files = common.Directory.ReadAnxFiles(segmenter.Configuration.DataFolder("Develop")).toList
     val pw = new java.io.PrintWriter(new File("logErrorLevel"))
     val sentences = files.map(f => {
         val sentence = AnxReader.ReadAnalyzedSentence(f)
         sentence
     } )
    val tags = sentences.map(t => t.sentenceWithLevel.map( s => BaseSegment.createTaggedSegment(s).GetTag))
    sentences.map(t => t.sentenceWithLevel.map( s => (BaseSegment.createTaggedSegment(s).GetTag,s))).flatten.foreach(f => {

   // println("Count type tags")
    val groupOfTags = tags.flatten.groupBy(f => f.simpleTag ).map(f => (f._1,f._2.length)).toList
    val groupOfLevels = tags.flatten.groupBy(f => f.Level ).map(f => (f._1,f._2.length)).toList
    //println(groupOfTags.length)
    groupOfTags.sortBy(f => f._2).foreach(f => println("Tag :" + f._1+ " count :" + f._2))
    //println("Max level "  + groupOfLevels.maxBy(f => f._1))
    groupOfLevels.sortBy(f => f._2).foreach(f => println("Tag :" + f._1+ " count :" + f._2))
    
     val groupOfDouble = tags.flatten.groupBy(f => f.tag).map(f => (f._1,f._2.length)).toList
     val count = groupOfDouble.map(f => f._2).sum
    groupOfDouble.sortBy(f => f._2).reverse.foreach(f => println("tag " + f._1 + " / " + Math.log(f._2.doubleValue / count).toString))
   })
   }
}
