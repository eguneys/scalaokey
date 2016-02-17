scalaVersion := "2.11.7"

libraryDependencies ++= List(
  "org.scalaz" %% "scalaz-core" % "7.1.6",
  "org.specs2" %% "specs2-core" % "3.6" % "test",
  "com.github.ornicar" %% "scalalib" % "5.3"
)


testOptions := Seq(Tests.Argument("xonly"))

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked")
