import sbt._
import Keys._

object HelloBuild extends Build {

lazy val root = Project(id = "SegmentAnalyzer", base = file("."))
    .aggregate(common, ruleClause, xmlLib, segmenter)

      lazy val common = Project(id = "Common",
                            base = file("./Common")
                            )
                            
        lazy val ruleClause = Project(id = "RuleClause",
                            base = file("./RuleClause"))  dependsOn (common)
                            
        
           lazy val xmlLib = Project(id = "xmlLib",
                            base = file("./XmlLib"))  dependsOn(common, ruleClause)
      
                lazy val segmenter = Project(id = "segmenter",
                            base = file("./Segmenter"))  dependsOn(common, ruleClause, xmlLib)
                          }