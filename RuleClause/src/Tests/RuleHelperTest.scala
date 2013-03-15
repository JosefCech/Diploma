package Tests

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common.{AnalyzedSegment,MorfWord , Boundary, PureSegment}
import scala.xml.{Node}
import Rules.{SegmentCondition , RuleHelper}

@RunWith(classOf[JUnitRunner])
class RuleHelperTest extends FunSuite  {
  test("update segmentcondition") {
    val att = List[(String,String)]{("Att","Value")}
   val test =  new Rules.ConditionSegment {
      attributes = att
    }
   1
  }
   test("Template node parse") {
       def xml : Node =
                <Template>
                  <Segment>
                    <Attribute Name="ActiveVerb" Value="1" />
                  </Segment>
                  <Boundary  Tag="J^" />
                  <Segment>
                    <Attribute Name="ActiveVerb" Value="0" />
                  </Segment>
                 </Template>
         // println(Rules.RuleHelper.GetTemplate(xml).zipWithIndex)
        1
    }

   test("Rule Parser") {
    def xml: Node =
               <Rule Priority="1">
                <Template>
                  <Segment>
                    <Attribute Name="ActiveVerb" Value="1" />
                  </Segment>
                  <Boundary  Tag="J^" />
                  <Segment>
                    <Attribute Name="ActiveVerb" Value="0" />
                  </Segment>
                 </Template>
                <Effect Segment="JoinToClause" Level="Actualevel" EffectOn="3" />
                </Rule>

    println(Rules.RuleHelper.parseRule(xml).toString)
   }

  
}
