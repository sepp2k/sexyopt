name := "sexyopt"

organization := "com.github.sepp2k"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.1"

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

useGpg := true

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { x => false }

homepage := Some(url("https://github.com/sepp2k/sexyopt"))
licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php"))
scmInfo := Some(ScmInfo(url("https://github.com/sepp2k/sexyopt"), "scm:git:git@github.com:sepp2k/sexyopt.git"))
developers := List(
    Developer(id = "sepp2k", name = "Sebastian Hungerecker", email = "sepp2k@googlemail.com", url = url("https://github.com/sepp2k/"))
)