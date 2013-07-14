package StatisticModul.Models

import common.Tag
import Anx.AnxReader

trait LoaderTags {

   def LoadTags : List[List[Tag]] = {
     val files = common.Directory.ReadAnxFiles(segmenter.Configuration.DataFolder("Develop")).toList
     val data = files.map(f => {
        val sentence = AnxReader.ReadAnalyzedSentence(f)
     //   sentence.sentenceWithData.foreach(f => println( "Clause / Level" + f.clause + "/" + f.level))
      //  println(sentence.analyzedSentence)
        sentence.getTagsOnly
     })
     data
   }
   
   def LoadTagsWithClause : List[List[(Int,Tag)]] = 
   {
     val files = common.Directory.ReadAnxFiles(segmenter.Configuration.DataFolder("Develop")).toList
     val data = files.map(f => {
        val sentence = AnxReader.ReadAnalyzedSentence(f)
        sentence.getTagsWithClause    
     })
     data
     
   }
}