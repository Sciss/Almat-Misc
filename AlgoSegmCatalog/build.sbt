lazy val baseName       = "AlgoSegmCatalog"
lazy val baseNameL      = baseName.toLowerCase

lazy val projectVersion = "0.1.0-SNAPSHOT"

// sonatype plugin requires that these are in global
ThisBuild / version       := projectVersion
ThisBuild / organization  := "de.sciss"
ThisBuild / versionScheme := Some("pvp")

lazy val commonSettings = Seq(
  description         := "Some visual materials",
  homepage            := Some(url(s"https://github.com/Sciss/$baseName")),
  licenses            := Seq("AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
  scalaVersion        := "3.0.2",
) 

// ---- dependencies ----

lazy val deps = new {
  val main = new {
    val fileUtil    = "1.1.5"
    val swingPlus   = "0.5.0"
  }
}

// ----

lazy val root = project.in(file("."))
  .settings(commonSettings)
  .settings(
    name := baseName,
    libraryDependencies ++= Seq(
      "de.sciss" %% "swingplus" % deps.main.swingPlus,
      "de.sciss" %% "fileutil"  % deps.main.fileUtil,
    ),
  )

