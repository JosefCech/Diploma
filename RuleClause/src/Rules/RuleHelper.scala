package Rules

import common.segment.TaggedSegment
import wordProperties.TagMatcher


object RuleHelper {
 
  def compareSegmentTemplate(segment : TaggedSegment, template : SegmentTemplate) : Boolean = {

    val matchTag = !template.tags.split(";").filter(p => TagMatcher.Match(segment.GetTagString, p)).isEmpty
    if (matchTag && !template.words.isEmpty)
    {
      // find words
      matchTag
    }
    matchTag
  }
   
  
}
