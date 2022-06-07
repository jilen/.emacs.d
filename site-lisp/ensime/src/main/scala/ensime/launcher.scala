// Copyright: Sam Halliday
// License: GPLv3+
package ensime

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardOpenOption.{ APPEND, CREATE, TRUNCATE_EXISTING }
import java.util.UUID
import java.io.File

// this is the code responsible for generating the content of the launcher
// script, which is the primary output of the compiler plugin and a dependency
// of the CLI.
object Launcher {
  val pluginName = "ensime"
  val cacheDir = sys.props("user.home") + "/.cache/ensime/"

  def mkScript(userSettings: List[String]): String = {
    val ensimeJar = userSettings.find(_.matches(s"^-Xplugin:.*${pluginName}.*[.]jar$$")).head.stripPrefix("-Xplugin:")

    val userName = sys.props("user.name")
    val userDir = sys.props("user.dir")
    val userJava = sys.props("java.home") + "/bin/java"

    // release as much memory back to the OS as possible to keep our overhead low
    // https://stackoverflow.com/questions/30458195
    val javaVersion = sys.props("java.version").stripPrefix("1.").takeWhile(_.isDigit).toInt
    val javaFlags = Nil

    // could capture envvars behind an allow-list
    var templ = getResourceAsString("ensime/launcher.sh")

    var replacements = Map(
      "__USERDIR__" -> userDir,
      "__USER__" -> userName,
      "__JAVA__" -> (userJava :: javaFlags).mkString(" "),
      "__ENSIME_JAR__" -> ensimeJar,
      "__USER_SETTINGS__" -> userSettings.map(s => "\"" + s + "\"").mkString(" ")
    )

    // the hash should be common to all source files that require the same compiler parameters
    val hash = UUID.nameUUIDFromBytes(replacements.values.toList.sorted.mkString.getBytes("utf-8")).toString
    replacements = replacements + ("__HASH__" -> hash)

    replacements.foreach { case (k, v) => templ = templ.replace(k, v) }

    templ
  }

  // writes the ensime file for the given source file and appends a reverse
  // lookup onto the output directory. Only writes the file if it changed.
  def write(file: File, target: File, launcher: String): Unit = {
    val src = file.getAbsolutePath()
    val exe = Path.of(cacheDir + src)
    if (Files.notExists(exe) || Files.readString(exe) != launcher) {
      Files.createDirectories(exe.getParent)
      Files.writeString(exe, launcher, CREATE, TRUNCATE_EXISTING)
      exe.toFile.setExecutable(true)
    }

    // every source file gets an empty file entry, allowing reverse lookup from
    // target directories to the sources that are relevant. Note that this file
    // can get stale! Source files that are later removed (either physically or
    // from the working set) will still show up as reverse lookups.
    //
    // There is no way to workaround this except for the user to purge the
    // entries, which can be managed by the build tool on a "clean" task.
    val lookup = Path.of(cacheDir + target.getAbsolutePath())
    if (Files.notExists(lookup)) {
      Files.createDirectories(lookup.getParent)
      Files.writeString(lookup, "", CREATE)
    }
    val srcs = Files.readAllLines(lookup)
    if (!srcs.contains(src))
      Files.writeString(lookup, src + "\n", APPEND)
  }

  private def getResourceAsString(res: String): String = {
    val is = getClass.getClassLoader.getResourceAsStream(res)
    try {
      val baos        = new java.io.ByteArrayOutputStream()
      val data        = Array.ofDim[Byte](2048)
      var len: Int    = 0
      def read(): Int = { len = is.read(data); len }
      while (read() != -1)
        baos.write(data, 0, len)
      baos.toString("UTF-8")
    } finally is.close()
  }
}
