
name := "cats-api-demo"

version := "0.1"

scalaVersion := "2.12.10"

val http4sVersion = "0.23.23"

// Add the version for ScalaTest
val scalatestVersion = "3.2.9"

lazy val protobuf =
  project
    .in(file("protobuf"))
    .settings(
      name := "protobuf",
      scalaVersion := scalaVersion.value
    )
    .enablePlugins(Fs2Grpc)

lazy val root =
  project
    .in(file("."))
    .settings(
      name := "root",
      scalaVersion := scalaVersion.value,
      libraryDependencies ++= Seq(
        "io.grpc" % "grpc-netty-shaded" % scalapb.compiler.Version.grpcJavaVersion,
        "org.typelevel" %% "cats-core" % "2.0.0",
        "org.scalatest" %% "scalatest" % scalatestVersion % Test  // Add ScalaTest dependency
      ),
      testFrameworks += new TestFramework("org.scalatest.tools.Framework") // Use ScalaTest framework
    )
    .dependsOn(protobuf)
