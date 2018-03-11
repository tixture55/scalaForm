name := """test-scala"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  "org.scalatra.scalate" %% "scalate-core" % "1.7.0",
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test,
  "mysql" % "mysql-connector-java" % "5.1.35"
)

unmanagedResourceDirectories in Compile += baseDirectory.value / "app" / "views"

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
