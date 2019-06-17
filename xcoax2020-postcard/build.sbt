lazy val baseName   = "xCoAx2020-Postcard"
lazy val baseNameL  = baseName.toLowerCase

lazy val projectVersion = "0.1.0-SNAPSHOT"

lazy val deps = new {
  val main = new {
    val fileUtil = "1.1.3"
    val fscape   = "2.26.2"
    val neural   = "2.4.0"
  }
}

lazy val commonSettings = Seq(
  version            := projectVersion,
  organization       := "de.sciss",
  homepage           := Some(url(s"https://git.iem.som/sciss/$baseNameL")),
  description        := "Algorithmic Postcard",
  licenses           := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt")),
  scalaVersion       := "2.13.0",
  scalacOptions     ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xlint:-stars-align,_", "-Xsource:2.13"),
)

// ---- sub-projects ----

lazy val root = Project(id = baseNameL, base = file("."))
  .settings(commonSettings)
  .settings(
    name := baseName,
    libraryDependencies ++= Seq(
      "de.sciss" %% "fileutil"       % deps.main.fileUtil,
      "de.sciss" %% "fscape-core"    % deps.main.fscape,
      "de.sciss" %  "neuralgas-core" % deps.main.neural,
    )
  )
