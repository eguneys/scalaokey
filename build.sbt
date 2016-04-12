scalaVersion := "2.11.8"

libraryDependencies ++= List(
  "org.scalaz" %% "scalaz-core" % "7.1.7",
  "org.specs2" %% "specs2-core" % "3.6" % "test",
  "com.github.ornicar" %% "scalalib" % "5.4"
)


testOptions := Seq(Tests.Argument("xonly"))

resolvers ++= Seq(
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases")

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-Ybackend:GenBCode", "-Ydelambdafy:method", "-target:jvm-1.8")
