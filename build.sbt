sbtPlugin := true

name := "scala_lxc"

description := "Small lxc wrapper to be used for my scala projects"

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.8.5")
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.3")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "1.1")
