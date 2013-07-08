package ClauseEstimate


import Anx.RulesReader
import Rules.Rule
import java.io.File
import segmenter.Configuration
import common.segment.TaggedSegment
import Rules.RuleAutomata


object RuleHandler {
  def rules : List[Rule] = RulesReader.ReadRules(new File(Configuration.RuleFile))
  
  def applyRules(sentence : ClauseAnalyzedSentence) : ClauseAnalyzedSentence = {
    
    val segments = sentence.analyzedSegments.map(p => new TaggedSegment(p)).toList
    val matches = rules.map(rule => RuleAutomata.conditionMatch(segments.zipWithIndex, rule))
    
    sentence.applyMatches(matches)
    sentence
  }
}