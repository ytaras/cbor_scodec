name := "cbor_scodec"

version := "1.0"

scalaVersion := "2.11.7"

//scalacOptions += "-Xlog-implicits"

libraryDependencies ++= Seq(
  "org.scodec" %% "scodec-core" % "1.8.3",
  //  "com.chuusai" %% "shapeless" % "2.2.5",
  "co.nstant.in" % "cbor" % "0.7" % "test",
  "eu.timepit" %% "refined-scodec" % "0.3.3",
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.12" % "0.3.1" % "test",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)