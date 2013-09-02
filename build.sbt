
name := "compiler-test"

version := "1.0-SNAPSHOT"

scalaVersion := "2.9.1"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _)

libraryDependencies += "org.specs2" %% "specs2" % "1.7.1"

scalacOptions in console in Compile <+= (packageBin in Compile) map { p =>
  "-Xplugin:"+p
}

scalacOptions in Test <+= (packageBin in Compile) map { p =>
  "-Xplugin:"+p
}

