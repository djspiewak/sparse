/*
 * Copyright 2015 Daniel Spiewak
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import com.typesafe.sbt.SbtGit._

organization := "com.codecommit"

name := "stream-parsers"

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

// I prefer not to hinder my creativity by predicting the future
git.baseVersion := "master"

bintraySettings