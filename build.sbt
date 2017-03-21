// Scala and library versions go here

val scala212Version = "2.12.1"

lazy val faacets = (project in file("."))
  .settings(moduleName := "faacets")
  .settings(faacetsSettings)
  .settings(noPublishSettings)
//  .aggregate(core) no dependencies yet
//  .dependsOn(core)

lazy val docs = (project in file("docs"))
  .enablePlugins(MicrositesPlugin)
  .settings(moduleName := "faacets-docs")
  .settings(faacetsSettings)
  .settings(docsSettings)
//  .dependsOn(core) no dependencies yet

lazy val docsSettings = Seq(
  micrositeName := "Faacets",
  micrositeDescription := "A library for Bell expressions",
  micrositeAuthor := "Jean-Daniel Bancal, Denis Rosset",
  micrositeBaseUrl := "/faacets", // baseurl is present in the `INSTALL.md` instructions
  micrositeDocumentationUrl := "/faacets/",
  micrositeGithubOwner := "denisrosset",
  micrositeGithubRepo := "faacets-gamma",
  micrositeCDNDirectives := microsites.CdnDirectives(
    jsList = List(
      "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
    )
  ),
  micrositePalette := Map(
    "brand-primary"     -> "#E05236",
    "brand-secondary"   -> "#3F3242",
    "brand-tertiary"    -> "#2D232F",
    "gray-dark"         -> "#453E46",
    "gray"              -> "#837F84",
    "gray-light"        -> "#E3E2E3",
    "gray-lighter"      -> "#F4F3F4",
    "white-color"       -> "#FFFFFF"),
  micrositeConfigYaml := microsites.ConfigYml(
    yamlCustomProperties = Map("baseUrl" -> "/faacets")
  ),
  fork in tut := true
)

lazy val faacetsSettings = buildSettings ++ commonSettings ++ publishSettings

lazy val buildSettings = Seq(
  organization := "com.faacets",
  scalaVersion := scala212Version
//  crossScalaVersions := Seq(scala211Version, scala210Version) no cross-build yet
)

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions.diff(Seq(
    "-Xfatal-warnings"
  )),
  resolvers ++= Seq(
    Resolver.url("spirejars", url(file("spirejars").toURI.toASCIIString))(Resolver.ivyStylePatterns), // TODO: remove use of Spire prerelease
    "bintray/non" at "http://dl.bintray.com/non/maven",
    "bintray/denisrosset/maven" at "https://dl.bintray.com/denisrosset/maven",
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")
  )
) ++ warnUnusedImport

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/denisrosset/faacets-gamma")),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  pomExtra := (
    <scm>
      <url>git@github.com:denisrosset/faacets-gamma.git</url>
      <connection>scm:git:git@github.com:denisrosset/faacets-gamma.git</connection>
    </scm>
    <developers>
      <developer>
        <id>denisrosset</id>
        <name>Denis Rosset</name>
        <url>http://github.com/denisrosset/</url>
      </developer>
      <developer>
        <id>jdbancal</id>
        <name>Jean-Daniel Bancal</name>
        <url>http://github.com/jdbancal/</url>
      </developer>
    </developers>
  ),
  bintrayRepository := "maven",
  publishArtifact in Test := false
)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)

lazy val commonJvmSettings = Seq(
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
) ++ selectiveOptimize

  // -optimize has no effect in scala-js other than slowing down the build
// do not optimize on Scala 2.10 because of optimizer bugs (cargo-cult setting
// from my experience with metal)
lazy val selectiveOptimize = 
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 10)) => Seq()
      case Some((2, 11)) => Seq("-optimize")
      case Some((2, 12)) => Seq()
      case _ => sys.error("Unknown Scala version")
    }
  }

lazy val warnUnusedImport = Seq(
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 10)) =>
        Seq()
      case Some((2, n)) if n >= 11 =>
        Seq("-Ywarn-unused-import")
    }
  },
  scalacOptions in (Compile, console) ~= {_.filterNot("-Ywarn-unused-import" == _)},
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
)

