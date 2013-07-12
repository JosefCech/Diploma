package statistic

import common.segment.TaggedSegment
import javax.swing.text.Segment

trait StatsClauseModel {

   def train(data: List[TaggedSegment]) : List[((Int,String), Double)]
   
   def estimate(data : List[TaggedSegment]) : List[Segment]
}