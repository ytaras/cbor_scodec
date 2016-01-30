name := "cbor_scodec"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scodec" %% "scodec-bits" % "1.0.12",
  "com.chuusai" %% "shapeless" % "2.2.5",
  "co.nstant.in" % "cbor" % "0.7" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
)