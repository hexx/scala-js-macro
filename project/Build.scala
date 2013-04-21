import sbt._,Keys._

object Build extends Build {
  lazy val baseSettings = Seq(
    scalaVersion := "2.10.1",
    organization := "com.github.hexx",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:_"
    )
  )

  lazy val llc4 = Project(
    id = "llc4",
    base = file("llc4")
  ).settings(
    baseSettings ++ seq(
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _),
      initialCommands in console += Seq(
      ).map("import " + _ + "\n").mkString
    ) : _*
  )

  lazy val llc4Example = Project(
    id = "llc4-example",
    base = file("llc4-example")
  ).settings(
    baseSettings: _*
  ).dependsOn(llc4)
}
