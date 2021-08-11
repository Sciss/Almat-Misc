lazy val baseName       = "PraiseOfEquiform"
lazy val baseNameL      = baseName.toLowerCase
lazy val projectVersion = "0.1.0-SNAPSHOT"
lazy val gitRepoHost    = "github.com"
lazy val gitRepoUser    = "Sciss"

lazy val root = project.in(file("."))
  .settings(
     name               := baseName,
     organization       := "de.sciss",
     homepage           := Some(url(s"https://$gitRepoHost/$gitRepoUser/$baseName")),
     description        := "Some algorithmic treatment of a text",
     licenses           := Seq("AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
     scalaVersion       := "3.0.1",
     libraryDependencies ++= Seq(
       "de.sciss" %% "linkernighantsp" % deps.linKernighan,
     )
  )

lazy val deps = new {
  val linKernighan = "0.1.3"
}

