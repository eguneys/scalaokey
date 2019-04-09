scalaVersion := "2.12.6"

libraryDependencies ++= List(
  "org.scalaz" %% "scalaz-core" % "7.2.23",
  "org.specs2" %% "specs2-core" % "4.2.0" % "test",
  "org.specs2" %% "specs2-scalaz" % "4.2.0" % "test",
  "com.github.ornicar" %% "scalalib" % "6.6"
)


testOptions := Seq(Tests.Argument("xonly"))

resolvers ++= Seq(
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases")

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-Ywarn-unused-import", "-Ydelambdafy:method", "-target:jvm-1.8")

publishTo := Some(Resolver.file("file", new File(sys.props.getOrElse("publishTo", ""))))
