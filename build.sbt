name := "encranion-present"

version := "0.1"

scalaVersion := "2.12.4"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.11",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.11" % Test,
  "com.typesafe" % "config" % "1.3.2",
  "com.github.purejavacomm" % "purejavacomm" % "1.0.2.RELEASE"
)
mainClass in assembly := Some("com.encranion.present.PresenterApp")
