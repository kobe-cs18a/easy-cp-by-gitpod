name := "tfpl"
version := "1.0"

scalaVersion := "2.12.11"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"

logBuffered in Test := false

parallelExecution in Test := false

resourceDirectory in Compile := baseDirectory.value / "data"

resourceDirectory in Compile := baseDirectory.value / "docs"

testOptions in Test ++= Seq(
  Tests.Argument(TestFrameworks.ScalaTest, "-fWDT", s"docs/test-report_${name.value}.txt","-eNDXEHLO","-C", "SampleReporter")
  )


