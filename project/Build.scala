import sbt._
import Keys._

object ScalaExtractorBuild extends Build {
/*
  lazy val root = Project(
      id = "root",
      base = file(".")
  )
  */
  lazy val lib = Project(
	id = "lib",
	base = file("lib")
  )
  
  lazy val samples = Project(
    id = "samples",
    base = file("samples")
  ) dependsOn(lib)
  
}
