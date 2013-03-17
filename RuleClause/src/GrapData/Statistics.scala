package GrapData

import Main.Clauser
import common.Directory
import java.io.File
import Anx.AnxReader
import Anx.AnxReader
import common.TaggedSegment


object Statistics extends App {
 
  private def segDataFolder = "../GoldenData/Seg"
  private def goldenDataFolder = "../GoldenData/OutputGolden"
  
  def getFilesAnx(folder : String) : List[File]= {
    println(folder)
    Directory.ReadAnxFiles(folder)
  }
   
  def getFilesSeg(folder : String) : List[File] = {
    Directory.ReadSegFiles(folder)
  }
  override def main(args: Array[String]){
    def filesAnxParsed : List[File] = getFilesAnx(goldenDataFolder)
    def filesGoldenData: List[File] = getFilesSeg(segDataFolder) 
    val sentences = filesAnxParsed.map(f => (AnxReader.ReadSentence(f))).map( t => t.map(f => new TaggedSegment(f)))
    val tagSegments = sentences.map(f =>  f.map(s => s.GetTag))
    val clause = sentences.map(s => s.groupBy(c => c.segment.clause)).flatten.map()
    
  }
  
  protected def baseSegmentsStatistic(tagSentences : List[List[String]]){
    val tagSegments = tagSentences.flatten
    val tagSegmentsCount : Float = tagSegments.size
    val grouppedTagSegments = tagSegments.groupBy(f => f).map(f => (f._1,f._2.size / tagSegmentsCount))
    printStatistic(grouppedTagSegments)
    // val analyzedSum = grouppedTagSegments.toList.sortBy(f => f._2).filter(p => p._2 < 0.001 && p._2 > 0.0001).map( p => p._2).sum
    // println(analyzedSum)  
    // println("Anx files :" + filesAnxParsed.size.toString)
  }
  protected def baseSegmentsStatistic(tagSentences : List[List[String]], grouppedOption : (Int,String) , viewOption : Int, text : String){
    val setTags = tagSentences.flatten.filter(t => t.substring(grouppedOption._1,grouppedOption._1 + grouppedOption._2.size) == grouppedOption._2)
    val setTagsCount : Float = setTags.size
    val groupped = setTags.groupBy(f => f).map(f => (f._1,f._2.size / setTagsCount))
    println(text)
    println("Whole count  " + setTagsCount.toString)
    printStatistic(groupped)
  }
  
  protected def uniGrams(tagSegments : List[List[String]]) : Unit ={
    baseSegmentsStatistic(tagSegments)
    baseSegmentsStatistic(tagSegments,(0,"S0"),2, "Segments on 0th level")
    baseSegmentsStatistic(tagSegments,(0,"S0C"),2, "Segments on 0th level with verb")
    baseSegmentsStatistic(tagSegments,(0,"S1"),2, "Segements on 1st level")
    baseSegmentsStatistic(tagSegments,(0,"S1C"),2, "Segements on 1st level with verb")
  }
  protected def printStatistic( groupped : Map[String,Float] ) = {
    groupped.toList.sortBy(f => f._2).foreach(f => println("Tag :" + f._1.toString() + " percent : " + (f._2).toString ))   
  }
}
