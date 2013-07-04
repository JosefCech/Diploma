package Rules

import common.segment.TaggedSegment


class RuleAutomata {
 
  def ruleMatches( sentence : List[TaggedSegment], rule : Rule ) : List[MatchEffect] = {
    if (!rule.isEmptyCondition)
    {
    val firstSegments = sentence.zipWithIndex.filter( s => RuleHelper.compareSegmentTemplate(s._1, rule.condition.segmentTemplate.head) && 
                                                           rule.numOfConditionObject <=  sentence.length - s._2)
    
    if (!firstSegments.isEmpty)                                                       
    {
      ruleMatch(sentence.zipWithIndex,firstSegments,rule,List[MatchEffect]())  
    } 
    List[MatchEffect]()
    }
    List[MatchEffect]()
  }
  
  def ruleMatch(sentence : List[(TaggedSegment, Int)] , firstSegment : List[(TaggedSegment,Int)], rule : Rule, acc : List[MatchEffect]) : List[MatchEffect] = {
    if (firstSegment.isEmpty) {
      acc
    }
    else {
      if (sentence.head._2 == firstSegment.head._2)
      {
        val matchEffect = conditionMatch(sentence,rule)
        ruleMatch(sentence.tail,firstSegment.tail, rule, matchEffect :: acc)
      }
      else
      {
        ruleMatch(sentence.tail,firstSegment,rule,acc)
      }
    }
  }
  
  def conditionMatch(sentence : List[(TaggedSegment, Int)], rule : Rule) : MatchEffect = {
    
     def conditionMatch(sentence : List[(TaggedSegment, Int)], condition : List[SegmentTemplate], prevSegmentTemplate : SegmentTemplate , acc : List[(Int, List[TaggedSegment])])  
     : List[(Int, List[TaggedSegment])] = {
       
       if (condition.isEmpty) // create MatchEffect
       {  
         acc
       }
       else if (sentence.isEmpty)
       {
         List[(Int, List[TaggedSegment])]()
       }
       else 
       {
            if (RuleHelper.compareSegmentTemplate(sentence.head._1, condition.head))
            {
              val nextIndex = acc.head._1 + 1
              var matchEffect = conditionMatch(sentence.tail, condition.tail, condition.head, (nextIndex,List(sentence.head._1)) :: acc)
              if (matchEffect.isEmpty && prevSegmentTemplate.isGroup && RuleHelper.compareSegmentTemplate(sentence.head._1, prevSegmentTemplate))
              {
               val prevHead = acc.head 
               val newAccHead =  (prevHead._1, sentence.head._1 :: prevHead._2)
               matchEffect = conditionMatch(sentence.tail, condition.tail, prevSegmentTemplate, newAccHead :: acc)
              }
              matchEffect
            }
            else {
              List[(Int, List[TaggedSegment])]()
            }
       } 
     } 
    
     val matchData = conditionMatch(sentence,rule.condition.segmentTemplate,new SegmentTemplate(),List[(Int, List[TaggedSegment])]())
     if (matchData.isEmpty) {
       new MatchEffect(-1,-1)
     }
     else {
       createMatchEffect(matchData,rule.effect)
     }
  }
  
  private def createMatchEffect(data : List[(Int, List[TaggedSegment])], effect : Effect) : MatchEffect = {
   // find effect on
   val indexObject : (Int,List[TaggedSegment]) = getObject(effect.effectOn, data)
   val clauseChange : Int =  getClauseNum(effect, data) 
   new MatchEffect(indexObject._1,clauseChange)
  }
  
  private def getObject(index : String , data : List[(Int, List[TaggedSegment])]) 
  : (Int, List[TaggedSegment]) = {
    val indexInt = getIndex(index)
    val result = data.filter(p => p._1 == indexInt)
    if (result.isEmpty) {
      (-1, List[TaggedSegment]())
    }
    else {
      result.head
    }
  }
  
  private def getIndex(index : String) : Int = {
	  evalRefInt(index)
  }
  
  
  private def evalRefInt(index : String) : Int = {
     try { 
    index.replace("$", "").toInt
    }
    catch {
      case  _ => -1; 
    }
  } 
  private def getClauseNum(effect : Effect ,data : List[(Int, List[TaggedSegment])]  ) : Int = {
   if (effect.clause.contains("$"))
   {
    val segmentData = getObject(effect.clause, data)
    if (segmentData._2.isEmpty) {
      0
    }
    else {
      segmentData._2.map(p => p.segment.clause).groupBy(f => f).head._1
    }
   }
   else 
   {
      getIndex(effect.clause)
   }
   }
 }
