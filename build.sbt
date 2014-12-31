import com.typesafe.sbt.SbtGit._

organization := "com.codecommit"

name := "stream-parsers"

version in ThisBuild := "0.1-SNAPSHOT"

scalaVersion := "2.11.4"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  "org.scalaz"        %% "scalaz-core"   % "7.1.0",
  "org.scalaz.stream" %% "scalaz-stream" % "0.6a",

  "org.specs2" %% "specs2-core" % "2.4.15" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")

logBuffered in Test := false

licenses += ("Apache-2.0", url("http://www.apache.org/licenses/"))

publishMavenStyle := true

versionWithGit

git.baseVersion := "0.1"

bintraySettings