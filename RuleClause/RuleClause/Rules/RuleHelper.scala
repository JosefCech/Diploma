package Rules

import common.segment.{Segment, TaggedSegment }

import wordProperties.TagMatcher
import common.{ RuleWord , MorfWord , Word}
import common.segment.BaseSegment



object RuleHelper {
 
  def compareSegmentTemplate(segment : TaggedSegment, template : SegmentTemplate) : Boolean = {

    val matchTag = !template.tags.split(";").filter(p => TagMatcher.Match(segment.GetTagString, p)).isEmpty
  
    if (matchTag && !template.words.isEmpty)
    {
       println(template.words.head.form)
       println(template.words.head.tags.head)
       println(segment.segment)
     val data =  RuleHelper.wordSequenceMatch(template.words,segment.segment)
     println(data)
     data
    } else {
     matchTag
    }
  }
 
  def  wordSequenceMatch(words : List[RuleWord], segment : Segment) : Boolean = {
    
     def wordsSequenceMatch(ruleWords : List[RuleWord], findWords : List[RuleWord], words : List[MorfWord], nextStart : List[MorfWord], tryMatch : Boolean ) 
     : Boolean = {
   
       if (ruleWords.isEmpty){
         true
       }
       else if (words.isEmpty) {
         false
       }
       else {
         if (tryMatch){
             if (ruleWords.head.equals(words.head))
             {
               wordsSequenceMatch(ruleWords.tail,ruleWords.head :: findWords,words.tail,nextStart,true)
             }
             else {
                 wordsSequenceMatch(findWords.reverse ::: ruleWords,List[RuleWord](),nextStart,List[MorfWord](),false)
             }
         }
         else {
            val nextRuleWords = {
               if (findWords.isEmpty) {
                 ruleWords
               }
               else {
                 findWords.reverse ::: ruleWords
               }
            }
            println(nextRuleWords.head.equals(words.head))
            if (nextRuleWords.head.equals(words.head))
            {
              wordsSequenceMatch(nextRuleWords.tail,List(nextRuleWords.head),words.tail,words.tail,true)
            }
            else 
            {
              wordsSequenceMatch(nextRuleWords,List[RuleWord](),words.tail,List[MorfWord](),false)
            }
        }
       }
       
     }
     def createMorfWords(s : Segment) : List[MorfWord] =  {
       s.words.map(f => f match 
        {
         case f : MorfWord => f
         case f : Word => new MorfWord(f.form,"")
        }
       ).toList
     }
     wordsSequenceMatch(words,List[RuleWord](),createMorfWords(segment),List[MorfWord](),false)
  }
  
  
  def applyRule(effects : List[MatchEffect], sentence : List[Segment]) : List[Segment] = {
     
    val sorted = effects.groupBy(f => f.effectOnIndex).map(p => p._2.reverse.head).toList.sortBy(f => f.effectOnIndex)
    
    sorted.foreach(f => sentence.apply(f.effectOnIndex).setClause(f.levelNum))
    sentence
      
    sentence;
  }
}
