package Tests

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Rules.{RuleHandler,Condition,RuleClauser,Effect,ConditionSegment,ConditionBoundary}
import common.{AnalyzedSegment, PureSegment, MorfWord, Boundary }
import Main.{Clauser}
import Main.RuleClauser


@RunWith(classOf[JUnitRunner])
class RuleHandlerTest extends FunSuite{
 test("count rules 3") {
   //RuleHandler.ruleNodes.foreach(f => println(f.toString))
   assert(1 == RuleHandler.rules.size)
 }
 
 test("compare segment condition - haveActiveVerb") {
      val condition = new Rules.ConditionSegment {
                                      attributes = List[(String,String)]{("ActiveVerb","1")}
                                      }
      val segment =  new AnalyzedSegment(
                        new PureSegment(
                              List( new MorfWord ("k","RR-------------") , new MorfWord ("nìmu","P5ZS3--3-------"), new MorfWord ("nedošlo","VpNS---XR-NA---"))
                              ,0)
                        )
      assert(RuleHandler.compareBaseSegment(segment, condition))
   }

   test("compare boundary condition") {
      val condition = new Rules.ConditionBoundary {
           boundary = List[(String,String)]{("a","J^")}
      }
      val segment =  new AnalyzedSegment(
                        new Boundary(
                              List( new MorfWord ("a","J^-------------"))
                              ,0)
                        )

      assert(RuleHandler.compareBaseSegment(segment, condition))
   }
   
    test("compare segment condition - haveActiveVerb='0' -false" ) {
      val condition = new Rules.ConditionSegment {
                                      attributes = List[(String,String)]{("ActiveVerb","1")}
                                      }
      val segment =  new AnalyzedSegment(
                        new PureSegment(
                              List( new MorfWord ("k","RR-------------") , new MorfWord ("nìmu","P5ZS3--3-------"))
                              ,0)
                        )
      assert(!RuleHandler.compareBaseSegment(segment, condition))
   }

      test("compare segment condition - try with boundary segment - false" ) {
      val condition = new Rules.ConditionSegment {
                                      attributes = List[(String,String)]{("ActiveVerb","1")}
                                      }
      val segment =   new AnalyzedSegment(
                        new Boundary(
                              List( new MorfWord ("a","J^-------------"))
                              ,0)
                        )
      assert(!RuleHandler.compareBaseSegment(segment, condition))
     }
   test("compare boundary condition - try with classic segment - false") {
      val condition = new Rules.ConditionBoundary {
          boundary = List[(String,String)]{("a","J^")}
      }
      val segment = new AnalyzedSegment(
                        new PureSegment(
                              List( new MorfWord ("k","RR-------------") , new MorfWord ("nìmu","P5ZS3--3-------"))
                              ,0)
                        )
 
      assert(!RuleHandler.compareBaseSegment(segment, condition))
   }
   
      test("compare boundary condition wrong form") {
      val condition = new Rules.ConditionBoundary {
         boundary = List[(String,String)]{("a","J^")}
      }
      val segment =  new AnalyzedSegment(
                        new Boundary(
                              List( new MorfWord ("a","J^-------------"))
                              ,0)
                        )

      assert(!RuleHandler.compareBaseSegment(segment, condition))
      }
   
   test("compare template test") {
    // println(this.testSegment.zipWithIndex)
     val matchAndEnd = 	RuleHandler.compareBaseSegment(this.testSegment.head._2, condition)
     val matchAndContinue = RuleHandler.compareTemplate(this.testSegmentDouble, this.testTemplate, Condition, 0)
     val noMatchAndEnd =  RuleHandler.compareTemplate(this.testSegmentNotMatch, this.testTemplate,  Condition, 0)
     val matchAndEnd2 =  RuleHandler.compareTemplate(this.testSegmentNotMatch, this.testTemplate2, Condition, 0)
     
     assert(matchAndEnd._1 == 0 && matchAndEnd._2 && matchAndEnd._3)
     assert(matchAndContinue._1 == 0 && matchAndContinue._2 && !matchAndContinue._3)
     assert(noMatchAndEnd._1 == 0 && !noMatchAndEnd._2 && noMatchAndEnd._3)
     assert(matchAndEnd2._1 == 0 && !matchAndEnd2._2 && !matchAndEnd2._3)  
     
     
   }
  
    test("applicable test on sentence") {
     var data = RuleHandler.applicableRule(this.testSegment, this.testRule)
     assert(data.size == 1)
     data = RuleHandler.applicableRule(this.testSegmentNotMatch, this.testRule)
     assert(data.size == 0)
     data = RuleHandler.applicableRule(this.testSegmentDouble, this.testRule)
     assert(data.size == 2)
   }
    
    test("test one sentence more rules") {
     var data = RuleHandler.applicableRules(this.testSegment, List( this.testRule , this.testRule2))
     assert(data.size == 1)
     data = RuleHandler.applicableRules(this.testSegmentNotMatch, List( this.testRule , this.testRule2))
     assert(data.size == 1)
     data = RuleHandler.applicableRules(this.testSegmentDouble, List( this.testRule , this.testRule2))
     assert(data.size == 2)
 
   }  
   

   def testRule : RuleClauser = new RuleClauser {
       template = testTemplate
       level = 1
       effect = new Effect {
         segment = "JoinToPrevious"
         level = "ActualLevel"
         effectOn = "2"
       }
   }
   def testRule2 : RuleClauser = new RuleClauser {
      template = testTemplate2
       level = 1
       effect = new Effect {
         segment = "new"
         level = "same"
         effectOn = "0"
       }
   }
   
   def testTemplate : List[Condition] = List (
       new Rules.ConditionSegment {
            attributes = List[(String,String)]{("ActiveVerb","1")}
      },
      new Rules.ConditionBoundary {
           form = "i|a"
           tag = "J^"
      },
       new Rules.ConditionSegment {
           attributes = List[(String,String)]{("ActiveVerb","0")}
      })
      
   def testTemplate2 : List[Condition] = List (
      new Rules.ConditionBoundary {
           form = "a"
           tag = "J^"
      },
       new Rules.ConditionSegment {
           attributes = List[(String,String)]{("ActiveVerb","1")}
      })
      
    def testTemplate3 : List[Condition] = List (
       new Rules.ConditionSegment {
            attributes = List[(String,String)]{("ActiveVerb","0")}
      },
      new Rules.ConditionBoundary {
           form = "i|a"
           tag = "J^"
      },
       new Rules.ConditionSegment {
           attributes = List[(String,String)]{("ActiveVerb","0")}
      })  
   def testSegment : List[(Int,AnalyzedSegment)]   = List(
                         (0 ,new AnalyzedSegment(
	                         new PureSegment(
	                              List( new MorfWord ("k","RR-------------") , new MorfWord ("nìmu","P5ZS3--3-------"), new MorfWord ("nedošlo","VpNS---XR-NA---"))
	                              ,0)
                        )),
                        (1,new AnalyzedSegment(
                        new Boundary(
                              List( new MorfWord ("a","J^-------------"))
                              ,0)
                        )),
                        (2,  new AnalyzedSegment(
	                        new PureSegment(
	                              List(  new MorfWord ("ní","P5ZS3--3-------"))
	                              ,0)
                        ))
   ) 
   def testSegmentDouble :  List[(Int,AnalyzedSegment)] = this.testSegment ::: List(
                         (3 ,new AnalyzedSegment(
	                         new PureSegment(
	                              List( new MorfWord ("k","RR-------------") , new MorfWord ("nìmu","P5ZS3--3-------"), new MorfWord ("nedošlo","VpNS---XR-NA---"))
	                              ,0)
                        )),
                        (4,new AnalyzedSegment(
                        new Boundary(
                              List( new MorfWord ("a","J^-------------"))
                              ,0)
                        )),
                        (5,  new AnalyzedSegment(
	                        new PureSegment(
	                              List(  new MorfWord ("ní","P5ZS3--3-------"))
	                              ,0)
                        ))
       )
   def testSegmentNotMatch : List[(Int,AnalyzedSegment)] = List(
                         (0 ,new AnalyzedSegment(
	                         new PureSegment(
	                              List( new MorfWord ("k","RR-------------") , new MorfWord ("nìmu","P5ZS3--3-------"), new MorfWord ("nedošlo","VpNS---XR-NA---"))
	                              ,0)
                        )),
                        (1,new AnalyzedSegment(
                        new Boundary(
                              List( new MorfWord ("a","J^-------------"))
                              ,0)
                        )),
                        (2 ,new AnalyzedSegment(
	                         new PureSegment(
	                              List( new MorfWord ("k","RR-------------") , new MorfWord ("nìmu","P5ZS3--3-------"), new MorfWord ("nedošlo","VpNS---XR-NA---"))
	                              ,0)
                        ))
   ) 
}
