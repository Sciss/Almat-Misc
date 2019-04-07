lazy val baseName       = "SpaceMaterialDetail"
lazy val baseNameL      = baseName.toLowerCase
lazy val appDescription = "Materials for the ALMAT @ MAST workshop 2019"
lazy val projectVersion = "0.1.0"
lazy val mimaVersion    = "0.1.0"
  
lazy val authorName     = "Hanns Holger Rutz"
lazy val authorEMail    = "contact@sciss.de"

// ---- dependencies ----

lazy val deps = new {
  val main = new {
    val fscape = "2.24.0"
    val lucre  = "3.11.1"
    val scopt  = "3.7.1"
  }
}

// ---- common ----

lazy val commonSettings = Seq(
  version            := projectVersion,
  organization       := "de.sciss",
  homepage           := Some(url(s"https://git.iem.at/sciss/$baseName")),
  licenses           := Seq("GNU Affero General Public License v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
  scalaVersion       := "2.12.8",
  scalacOptions ++= Seq(
    "-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint:-stars-align,_", "-Xsource:2.13"
  ),
  scalacOptions += "-Yrangepos",  // this is needed to extract source code
  updateOptions := updateOptions.value.withLatestSnapshots(false)
)

// ---- projects ----

lazy val root = project.withId(baseNameL).in(file("."))
  .settings(commonSettings)
  .settings(
    name        := baseName,
    description := appDescription,
    resolvers         ++= Seq(
      "Oracle Repository" at "http://download.oracle.com/maven",   // required for sleepycat
    ),
    libraryDependencies ++= Seq(
      "de.sciss"          %% "fscape-modules" % deps.main.fscape, // signal processing
      "de.sciss"          %% "fscape-views"   % deps.main.fscape, // signal processing
      "de.sciss"          %% "lucre-bdb"      % deps.main.lucre,  // object system (database backend)
      "com.github.scopt"  %% "scopt"          % deps.main.scopt,  // command line option parsing
    )
  )
