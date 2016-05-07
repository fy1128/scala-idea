name := "jdcoupon-sbt"

version := "1.0"

scalaVersion := "2.11.8"


libraryDependencies += "com.alibaba" % "fastjson" % "1.2.11"

mainClass in(Compile, run) := Some("JDCoupon")