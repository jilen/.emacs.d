organization := "com.fommil"

licenses := List(License.GPL3_or_later)

ThisBuild / crossScalaVersions := List(
  "3.2.0",
  "2.13.9",
  "2.12.15", // the version of scala used by sbt 1.6.2
  "2.12.17",
  "2.11.12"
)
ThisBuild / scalaVersion := "2.13.9"

val install = taskKey[Unit]("Install the ENSIME jar.")

lazy val ensime = (project in file(".")).settings(
  Compile / unmanagedSourceDirectories ++= {
    val dir = (Compile / scalaSource).value
    val Some((major, minor)) = CrossVersion.partialVersion(scalaVersion.value)
    val specific =
      if (major == 2 && minor <= 12) file(s"${dir.getPath}-2.12-") :: Nil
      else Nil

    file(s"${dir.getPath}-$major") :: specific
  },

  // scalacOptions += "-deprecation"
  scalacOptions ++= {
    if (scalaVersion.value.startsWith("2.11.")) List("-Xexperimental", "-target:jvm-1.8") else Nil
  },

  libraryDependencies ++= {
    if (scalaVersion.value.startsWith("3.")) Seq(
      "org.scala-lang" % "scala3-compiler_3" % scalaVersion.value
    ) else Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  },

  libraryDependencies ++= Seq(
    "com.facebook" % "nailgun-server" % "1.0.1",
    "org.ow2.asm"  % "asm"            % "9.2"
  ),

  crossTarget := target.value / s"scala-${scalaVersion.value}",

  // tests expect the jar here
  assembly / assemblyJarName := "ensime.jar",
  assemblyMergeStrategy := {
    case "rootdoc.txt" => MergeStrategy.discard
    case x => assemblyMergeStrategy.value(x)
  },

  install := {
    streams.value.log.info(s"Installing ${ensimeJar.value}")
    IO.write(ensimeJar.value, IO.readBytes(assembly.value))

    val plugin = file(s"""${sys.props("user.home")}/.sbt/1.0/plugins/EnsimePlugin.scala""")
    streams.value.log.info(s"Installing $plugin")
    IO.copyFile(file("project/EnsimePlugin.scala"), plugin)
  }
)

val lsp = project
  .settings(
    crossScalaVersions := Seq(),
    libraryDependencies ++= Seq(
      "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.16.0",
      "com.novocode" % "junit-interface" % "0.11" % Test,
      "junit" % "junit" % "4.13.2" % Test
    ),
    crossPaths := false, // https://github.com/sbt/junit-interface/issues/35
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-v"),

    scalacOptions += "-deprecation",

    // https://github.com/eclipse/lsp4j/issues/127#issuecomment-343781551
    scalacOptions += "-Xmixin-force-forwarders:false",

    assembly / mainClass := Some("ensime.EnsimeLsp"),
    assembly / assemblyJarName := "ensime-lsp.jar",
    assemblyMergeStrategy := {
      case "rootdoc.txt" => MergeStrategy.discard
      case x => assemblyMergeStrategy.value(x)
    },

    install := {
      val ensimeLspJar = file(s"""${sys.props("user.home")}/.cache/ensime/lib/ensime-lsp.jar""")
      streams.value.log.info(s"Installing ENSIME LSP to $ensimeLspJar")
      IO.write(ensimeLspJar, IO.readBytes(assembly.value))
    }
)
