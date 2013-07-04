package ClauseEstimate

import Rules.Rule
import Anx.RulesReader
 
object RuleHandler {
 
  def rules : List[Rule] = Anx.RulesReader()
}