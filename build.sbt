
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.10"

ThisBuild / githubOwner := "michel-medema"
ThisBuild / githubRepository := "scala-graph-tools"
ThisBuild / githubTokenSource := TokenSource.Environment("GITHUB_TOKEN")

lazy val scalatestVersion = "3.2.19"
lazy val scalamockVersion = "7.4.1"

lazy val root = (project in file("."))
  .settings(
    name := "scala-graph-tools",
    resolvers += Resolver.githubPackages("michel-medema", "scala-graph-tools"),
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % scalatestVersion,
      "org.scalatest" %% "scalatest" % scalatestVersion % "test",
      "org.scalamock" %% "scalamock" % scalamockVersion % Test,
      "io.github.twalgor" % "twalgor" % "1.0"
    )
  )