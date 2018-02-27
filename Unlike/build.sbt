lazy val baseName         = "Unlike"
lazy val baseNameL        = baseName.toLowerCase
lazy val projectVersion   = "0.2.0-SNAPSHOT"

lazy val commonSettings = Seq(
  version             := projectVersion,
  organization        := "de.sciss",
  description         := "A photo/video experiment",
  homepage            := Some(url(s"https://github.com/Sciss/$baseName")),
  scalaVersion        := "2.11.12",  // can't use 2.12 because of fscape-jobs
  licenses            := Seq(gpl2),
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint"),
  resolvers           += "Typesafe Releases" at "https://repo.typesafe.com/typesafe/maven-releases/",
  libraryDependencies ++= Seq(
    "de.sciss"          %% "fileutil"           % "1.1.3",
    "de.sciss"          %% "numbers"            % "0.1.4",
    "de.sciss"          %% "processor"          % "0.4.1",
    "com.mortennobel"   %  "java-image-scaling" % "0.8.6",  // includes jh filters
    "de.sciss"          %% "audiowidgets-swing" % "1.11.2",
    "de.sciss"          %% "desktop"            % "0.8.1",
    "de.sciss"          %% "guiflitz"           % "0.5.1",
    "de.sciss"          %% "play-json-sealed"   % "0.4.1",
    "de.sciss"          %% "kollflitz"          % "0.2.1",
    "com.github.scopt"  %% "scopt"              % "3.7.0",
    "de.sciss"          %% "scissdsp"           % "1.2.3",
    "de.sciss"          %% "scalaaudiofile"     % "1.4.6",
    "de.sciss"          %% "fscapejobs"         % "1.5.0"
  )
)

//lazy val cc_by_nc_nd = "CC BY-NC-ND 4.0" -> url("http://creativecommons.org/licenses/by-nc-nd/4.0/legalcode")
lazy val gpl2        = "GPL v2+"         -> url("http://www.gnu.org/licenses/gpl-2.0.txt")

lazy val root = Project(id = baseNameL, base = file("."))
  .settings(commonSettings)

// -------------

//mainClass in assembly := Some("de.sciss.unlike.Main")
//
//assemblyJarName in assembly := s"$baseName.jar"
