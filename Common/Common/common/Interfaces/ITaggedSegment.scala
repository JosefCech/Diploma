package common.Interfaces

import common.Tag

trait ITaggedSegment {
  def GetTagString : String
  def GetTag : Tag
}