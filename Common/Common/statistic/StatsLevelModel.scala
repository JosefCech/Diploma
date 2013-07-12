package statistic

import common.segment.{TaggedSegment, Segment}
import log.SimpleLog

trait StatsLevelModel extends SimpleLog {
def train(data: List[TaggedSegment]) : List[((Int,String), Double)]
   
   def estimate(data : List[TaggedSegment]) : List[Segment]
}