name := "Segment analyzer"

version := "0.1"

scalaVersion := "2.9.1"

publish := false




scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation",
"-unchecked")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.6.1" % "test" 
)



libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"

libraryDependencies ++= Seq( 
    "junit" % "junit" % "4.10",
    "com.novocode" % "junit-interface" % "0.10-M1" % "test"
    )