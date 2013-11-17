package StatisticModul


import common.sentence.AnxSentence
import DataObjects.EstimateSentence
import statistic.{Viterbi , LevelSegmentModel  }
import common.sentence.LevelEstimateSentence
import LevelEstimate.LevelStatisticAnalyzedSentence
import statistic.Models.UnigLevelModel
import statistic.Models.ConditionalLevelModel
import StatisticModul.Models.LevelModel
import StatisticModul.Models.ConditionalLevelModel
import StatisticModul.Models.ClauseModel
import StatisticModul.Models.ConditionalClauseModel
import statistic.Models.UnigClauseModel
import statistic.BaseModel.UnigModel
import statistic.BaseModel.ConditionalModel
import ClauseEstimate.ClauseStatisticAnalyzedSentence
import statistic.{ LevelViterbi , ClauseViterbi }
import StatisticModul.Models.LevelModelDiff
import StatisticModul.Models.ConditionalLevelModelDiff
import common.segment.Segment
import LevelEstimate.LevelStatisticAnalyzedSentenceDiff
import LevelEstimate.LevelAnalyzedSentence


class StatisticLevelEstimate extends LevelViterbi {
  
  override val getLevelModel : UnigModel = LevelModel
  override val getConditionModel : ConditionalModel = ConditionalLevelModel
  
  def StatisticEstimateLevel(sentence : AnxSentence ) : EstimateSentence = 
  {
   val tags = sentence.getTagsOnly
   val best = this.getBestPath(tags)
   new LevelStatisticAnalyzedSentence(sentence,best)
  }

  
}

class StatisticLevelEstimateDiff extends LevelViterbi {
  
  override val getLevelModel : UnigModel = LevelModel
  override val getConditionModel : ConditionalModel = ConditionalLevelModel
  
  def StatisticEstimateLevel(sentence : AnxSentence ) : EstimateSentence = 
  {
   val tags = sentence.getTagsOnly
   val best = this.getBestPath(tags)
   new LevelStatisticAnalyzedSentenceDiff(sentence,best)
  }
  

}
class StatisticClauseEstimate extends ClauseViterbi {
  
  override val getLevelModel : UnigModel = ClauseModel
  override val getConditionModel : ConditionalModel = ConditionalClauseModel
  
  def StatisticEstimateClause(sentence : AnxSentence ) : EstimateSentence = 
  {
   val tags = sentence.getTagsOnly
   val best = this.getBestPath(tags)
   new ClauseStatisticAnalyzedSentence(sentence,best)
  }
  
  def StatisticEstimateClause(sentence : EstimateSentence , ident : String) : EstimateSentence =  {
  	
  	val anxSentence = new AnxSentence(sentence.getEstimateSegments,ident)
  	this.StatisticEstimateClause(anxSentence)
  }

  
}