name := "Statistic project"

version := "0.1"

scalaVersion := "2.9.1"

scalaSource in Compile <<= baseDirectory(_ / "src")

publishTo := Some(Resolver.file("file",  new File( "../Application" )) )

scalaSource in Test <<= baseDirectory(_ / "test")

scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation",
"-unchecked")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.6.1" % "test" 
)

fork in run := true

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"

libraryDependencies ++= Seq( 
    "junit" % "junit" % "4.10",
    "com.novocode" % "junit-interface" % "0.10-M1" % "test",
    "org.scala-lang" % "scala-swing" % "2.9.1"
    )