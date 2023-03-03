package ensime

import java.io.File
import java.lang.management.ManagementFactory
import java.net.URI
import java.nio.file.{ FileSystems, Files, Path, StandardWatchEventKinds, WatchService }
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.StandardOpenOption.{ CREATE, TRUNCATE_EXISTING }
import java.util.{ List => JList, Timer, TimerTask }
import java.util.concurrent.CompletableFuture
import java.util.zip.ZipFile

import scala.concurrent.Future
import scala.jdk.CollectionConverters._
import scala.jdk.FutureConverters._
import scala.sys.process._
import scala.util.control.NonFatal

import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.messages.{ Either => LspEither }
import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.services._

object EnsimeLsp {

  private val cacheDir = new File(sys.props("user.home") + "/.cache/ensime/")

  def main(args: Array[String]): Unit = {
    val runtimeMXBean = ManagementFactory.getRuntimeMXBean()
    val cp = runtimeMXBean.getClassPath()
    val args = runtimeMXBean.getInputArguments.asScala

    System.err.println(s"Starting ENSIME LSP ($cp) [${args.mkString(" ")}]")

    if (cp.endsWith(".jar") && new File(cp).exists()) {
      ensimeLspJar = new File(cp)
      ensimeLspModified = ensimeLspJar.lastModified()
    }

    val server = new EnsimeLsp
    val launcher = LSPLauncher.createServerLauncher(server, System.in, System.out)
    val client = launcher.getRemoteProxy
    server.connect(client)
    launcher.startListening()

    if (!new File(cacheDir, sys.props("user.home")).exists()) {
      client.showMessage(
        new MessageParams(
          MessageType.Info,
          // to run the LSP the user must have already built the repository at least once
          "Welcome to ENSIME! Reload your build tool after installing the compiler plugin, and compile at least once."
        )
      )
    } else if (sys.env("PATH").split(File.pathSeparator).toList.find(d => new File(d, "nailgun").isFile).isEmpty) {
      client.showMessage(
        new MessageParams(
          MessageType.Info,
          // will appear on second use, if the user didn't follow all the README instructions
          "Install [Nailgun](https://github.com/facebook/nailgun) for a more performant experience."
        )
      )
    }
  }

  // be nice and shut down automatically if the user doesn't talk to us in a while
  @volatile private var heartbeat_ = System.currentTimeMillis()
  @volatile private var shutdowner = false
  @volatile private var ensimeLspJar: File = _
  @volatile private var ensimeLspModified = -1L
  private def heartbeat(): Unit = synchronized {
    if (ensimeLspModified > 0 && ensimeLspJar.exists() && ensimeLspJar.lastModified() != ensimeLspModified) {
      System.err.println("ENSIME LSP upgrade in process")
      sys.exit(0)
    }

    heartbeat_ = System.currentTimeMillis()

    val timeout = 60 * 60 * 1000L
    if (!shutdowner) {
      shutdowner = true
      val checker = new TimerTask {
        def run(): Unit = {
          if (System.currentTimeMillis() > (heartbeat_ + timeout)) {
            System.err.println("Shutting down ENSIME LSP due to inactivity")
            sys.exit(0)
          } else System.gc()
        }
      }
      new Timer("shutdowner", true).scheduleAtFixedRate(checker, 30000L, 30000L)
    }
  }
}

case class QuietExit(msg: String) extends Exception(msg)

// see https://github.com/eclipse/lsp4j/issues/321 regarding annotations
class EnsimeLsp extends LanguageServer with LanguageClientAware {

  private def async[A >: Null](f: => A): CompletableFuture[A] = {
    EnsimeLsp.heartbeat()

    import scala.concurrent.ExecutionContext.Implicits.global

    Future{
      try f
      catch {
        case QuietExit(msg) =>
          System.err.println(s"early exit: $msg")
          null
      }
    }.asJava.toCompletableFuture
  }

  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = async {
    val res = new InitializeResult

    val capabilities = new ServerCapabilities

    // this is inefficient, consider swapping to Incremental and applying diffs
    // as they are received by didChange.
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)

    capabilities.setHoverProvider(true)
    capabilities.setDefinitionProvider(true)
    capabilities.setCompletionProvider(
      new CompletionOptions(false, List(".").asJava)
    )

    val serverinfo = new ServerInfo
    serverinfo.setName("ENSIME")
    serverinfo.setVersion("TNG")

    res.setCapabilities(capabilities)
    res.setServerInfo(serverinfo)
    res
  }

  // contains the list of files that have been opened along with their content
  // as last communicated by the client (it may match what is on disk). It is
  // expensive to maintain this, since we can get full-file updates on every
  // character typed, but LSP doesn't give us reliable alternatives.
  @volatile private var openFiles: Map[File, String] = Map()

  private def uriToFile(s: String): Option[File] = {
    val uri = new URI(s)
    if (uri.getScheme == "file")
      Some(new File(uri.getPath))
    else
      None
  }

  private def uriToFile_[A](uri: String): File = uriToFile(uri) match {
    case Some(file) => file
    case None => throw new UnsupportedOperationException(s"${uri} is not a file")
  }

  // uses the ensime HASH to compute which open files are part of the current
  // active set (i.e. files which are part of the same artefact). A consequence
  // of this is that files which are changed but not part of the active set will
  // be ignored, which is an intentional design decision (the user is expected
  // to compile files all dependencies). The response does not contain the
  // input.
  private def activeSet(focus: File): Set[File] = launcher(focus) match {
    case None => Set()
    case Some(focusLauncher) => filesWithHash(launcherHash(focusLauncher)) - focus
  }

  private def filesWithHash(hash: String): Set[File] =
    openFiles.keySet.filter { f => launcher(f).map(launcherHash) == Some(hash) }

  private def launcher(focus: File): Option[File] = {
    val probe = new File(EnsimeLsp.cacheDir, focus.getAbsolutePath)
    if (probe.isFile) Some(probe)
    else None
  }
  private def launcherHash(ensime: File): String = {
    Files.readAllLines(ensime.toPath).asScala.find(_.startsWith("HASH=")).map(_.drop(5)) match {
      case Some(hash) => hash
      case None => throw new IllegalStateException(s"ENSIME file $ensime is corrupted")
    }
  }

  // like ensimeFile but also tries to use a "last best known" file which covers
  // the corner case of new files that haven't been compiled yet.
  private def ensimeExe(focus: File): File = {
    val exe = launcher(focus)
    if (exe.isDefined) {
      lastEnsimeExe = exe
      exe.get
    } else {
      lastEnsimeExe match {
        case Some(last) => last
        case None =>
          if (!shownHelp) {
            shownHelp = true
            client.showMessage(
              new MessageParams(
                MessageType.Warning,
                "The ENSIME launcher (created as a side effect of the compilation) was not found. Common problems:\n\n" +
                  "  1. the ENSIME plugin has not been installed for this project's version of Scala.\n" +
                  "  2. the build tool has not been reloaded or informed about ENSIME.\n" +
                  "  3. the project or file has not been cleanly compiled at least once.\n\n" +
                  "Please consult the ENSIME README for further instructions and to check compatibility."
              )
            )
          }
          throw QuietExit(s"ENSIME launcher not available for $focus")
      }
    }
  }
  @volatile private var shownHelp: Boolean = false
  @volatile private var lastEnsimeExe: Option[File] = None

  // shared between all ensime instances, this could be made cleaner by adding HASH
  private val tmp_prefix: String = s"/tmp/${sys.props("user.name")}/ensime/"

  // assumes that File is present in openFiles and will return the file as-is if
  // the in-memory representation matches what is on disk. Otherwise, a
  // temporary file is created (having the same name) and returned. Note that
  // this will break the ability to link compiler reporter messages back to
  // their original filenames.
  private def tmpIfDifferent(f: File): File = {
    val inmemory = openFiles.get(f).getOrElse(throw new IllegalStateException(s"expected $f to exist in ${openFiles.keySet}"))
    val disk = Files.readString(f.toPath)

    // maybe need to do some whitespace normalisation...
    if (inmemory == disk) f
    else {
      // we never clean these up
      val tmp = new File(tmp_prefix + f.getAbsolutePath)
      // System.err.println(s"writing temp file for $f as $tmp")
      tmp.getParentFile.mkdirs()
      Files.writeString(tmp.toPath, inmemory, CREATE, TRUNCATE_EXISTING)
      tmp
    }
  }

  private def ensime(mode: String, f: File, pos: Position): String =
    ensime(mode, f, s"${pos.getLine}:${pos.getCharacter}", true)

  private def ensime(mode: String, f: File, args: String, includeTarget: Boolean): String = {
    val exe = ensimeExe(f)
    val target = tmpIfDifferent(f)
    val active = activeSet(f).map(tmpIfDifferent(_))
    val context = active.mkString(" ")

    val stderr = new StringBuilder
    val processLogger = ProcessLogger(_ => (), stderr.append(_))

    val params = if (includeTarget) s"$target " else ""
    val command = s"$exe $mode $params$args $context"
    System.err.println(command)

    try command !! processLogger
    catch {
      // usually just means the file is uncompilable, which can be normal
      case NonFatal(_) => throw QuietExit(s"ensime launcher failed, source may be uncompilable, this can be expected")
    } finally {
      if (stderr.nonEmpty)
        System.err.println(stderr.toString)
    }
  }

  private def tokenAtPoint(txt: String, pos: Position): String = {
    val line = txt.split("\n")(pos.getLine)

    val buf = new StringBuilder

    var i = pos.getCharacter - 1
    while (i >= 0 && line(i).isLetter) {
      buf.append(line(i))
      i -= 1
    }
    buf.reverseInPlace()
    i = pos.getCharacter
    while (i < line.length && line(i).isLetter) {
      buf.append(line(i))
      i += 1
    }

    buf.toString
  }

  // the events of a FileWatcher do not include the full path, so we need a new
  // watcher for every directory that we want to watch. This is keyed by the
  // diagnostics file and the project hash.
  @volatile private var watchers: Map[(File, String), WatchService] = Map()

  {
    val watcherThread = new Thread("file-watcher") {
      override def run(): Unit = while (true) {
        watchers.foreach { case ((file, hash), watcher) =>
          val key = watcher.take()
          key.pollEvents().asScala.foreach { e =>
            e.context() match {
              case p: Path =>
                System.err.println(s"detected changes to $file")
                if (p.toString == "diagnostics.log") {
                  try diagnosticsCallback(file, hash)
                  catch {
                    case NonFatal(e) =>
                      System.err.println(s"error when calculating diagnostics: ${e.getMessage} ${e.getClass}")
                  }
                }
              case _ =>
            }
          }
          key.reset()
        }

        Thread.sleep(1000)
      }
    }
    watcherThread.setDaemon(true)
    watcherThread.start()
  }

  // invoked when the diagnostics.log file changes (may be deleted or empty)
  def diagnosticsCallback(log: File, hash: String): Unit = {
    val timestamp = if (!log.exists()) -1L else log.lastModified()
    val updates: List[(File, Diagnostic)] = {
      if (!log.exists()) Nil
      else Files.readString(log.toPath)
        .split("\u0000")
        .toList
        .map(_.split("\n").toList)
        .flatMap {
          case level :: path :: sline :: scol :: eline :: ecol :: msg_ =>
            val msg = msg_.mkString("\n")
            val severity = level match {
              case "WARNING" => DiagnosticSeverity.Warning
              case "ERROR" => DiagnosticSeverity.Error
              case _ => DiagnosticSeverity.Information
            }
            val file = new File(path)
            if (openFiles.contains(file) && file.exists() && file.lastModified() <= timestamp) {
              val range = new Range(new Position(sline.toInt - 1, scol.toInt - 1), new Position(eline.toInt - 1, ecol.toInt - 1))
              val diagnostic = new Diagnostic(range, msg)
              diagnostic.setSeverity(severity)
              Some(file -> diagnostic)
            } else None

          case _ => None
        }
    }

    System.err.println(s"${updates.size} diagnostics from the latest compile")

    // must send an empty list to clear the files
    val clean = filesWithHash(hash).map { f => (f, Nil)}.toMap

    // updates take precedence
    (clean ++ updates.groupBy(_._1)).foreach { case (file, ds) =>
      val diagnostics = ds.map(_._2).asJava
      System.err.println(s"publishing ${diagnostics.size} diagnostics for $file")
      client.publishDiagnostics(
        new PublishDiagnosticsParams(file.toPath().toUri().toString, diagnostics)
      )
    }
  }

  override def getTextDocumentService(): TextDocumentService = new TextDocumentService {
    // we only care about monitoring the active set
    override def didClose(p: DidCloseTextDocumentParams): Unit = {
      val f = uriToFile_(p.getTextDocument.getUri)
      System.err.println(s"CLOSED $f")

      synchronized {
        openFiles -= f
      }
    }
    override def didOpen(p: DidOpenTextDocumentParams): Unit = {
      val f = uriToFile_(p.getTextDocument.getUri)
      System.err.println(s"OPENED $f")
      val content = p.getTextDocument.getText

      synchronized {
        openFiles = openFiles + (f -> content)
      }

      launcher(f).foreach { ef =>
        val hash = launcherHash(ef)
        val df = new File(tmp_prefix, hash + "/diagnostics.log")

        synchronized {
          if (!watchers.contains((df, hash))) {
            System.err.println(s"registering a filewatcher for $df")
            val watcher = FileSystems.getDefault().newWatchService()

            import StandardWatchEventKinds._
            df.toPath.getParent.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY)

            watchers += ((df, hash) -> watcher)
            diagnosticsCallback(df, hash) // renders existing diagnostics
          }
        }
      }
    }
    override def didChange(p: DidChangeTextDocumentParams): Unit = {
      val f = uriToFile_(p.getTextDocument.getUri)
      // System.err.println(s"CHANGED $f")
      val content = p.getContentChanges.get(0).getText // Full means this is not a diff

      synchronized {
        openFiles = openFiles + (f -> content)
      }
    }

    override def didSave(p: DidSaveTextDocumentParams): Unit = ()

    // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion
    override def completion(params: CompletionParams): CompletableFuture[LspEither[JList[CompletionItem], CompletionList]] = async {
      val f = uriToFile_(params.getTextDocument.getUri)
      val pos = params.getPosition
      // editors can sometimes send completion requests in places other than
      // the defined trigger characters, so be a little protective against
      // that.
      val before = new Position(pos.getLine, pos.getCharacter - 1)
      val charBefore = openFiles(f).split('\n')(before.getLine)(before.getCharacter)
      val completions = if (charBefore != '.') {
        Nil
      } else {
        val output = ensime("complete", f, pos)
        output.split("\n").toList.map { sig =>
          val item = new CompletionItem
          item.setLabel(sig)
          item.setInsertTextFormat(InsertTextFormat.Snippet)

          val parsed = SigParser.parse(sig).stripImplicit
          val snippet = SigParser.snippet(parsed)
          item.setInsertText(snippet)

          if (parsed.isInfix) {
            // infix operators replace the dot with a space
            val edit = new TextEdit(new Range(before, pos), " ")
            item.setAdditionalTextEdits(List(edit).asJava)
          }

          item
        }
      }

      LspEither.forRight(new CompletionList(completions.asJava))

    }

    override def hover(params: HoverParams): CompletableFuture[Hover] = async {
      val f = uriToFile_(params.getTextDocument.getUri)
      val output = ensime("type", f, params.getPosition).trim()
      val markdownOutput = s"```scala\n$output\n```"
      val content = new MarkupContent("markdown", markdownOutput)
      new Hover(content)
    }

    override def definition(params: DefinitionParams): CompletableFuture[LspEither[JList[_ <: Location], JList[_ <: LocationLink]]] = async {
      val f = uriToFile_(params.getTextDocument.getUri)
      val output = ensime("source", f, params.getPosition)
      val defns = output.split("\n").toList.map { resp =>
        val parts = resp.split(":")
        if (parts.length != 2) {
          // debug why and when this happens... seen in the wild (scala.runtime.NonLocalReturnControl)
          throw QuietExit(s"unexpected launcher response $resp")
        }
        val file = if (parts(0).isEmpty) f.toString else parts(0).replace(tmp_prefix, "")
        val pos = new Position(0 max (parts(1).toInt - 1), 0)
        val range = new Range(pos, pos)

        val cleaned =
          if (file.contains("!")) extractZipEntry(file)
          else s"file://$file"
        // System.err.println(s"FOUND $cleaned")

        new Location(cleaned, range)
      }

      LspEither.forLeft(defns.asJava)
    }

    // given an ensime style entry "/foo/bar.jar!/baz/gaz.scala" extract the
    // entry into the tmp directory and return a File path to that entry for a
    // text editor to open naturally.
    private def extractZipEntry(uri: String): String = {
      val parts = uri.split("!")
      val name = parts(0)
      val archive = new ZipFile(name)
      val path = parts(1).stripPrefix("/")
      val out = Path.of(tmp_prefix + name + "/" + path)
      out.getParent().toFile().mkdirs()
      // System.err.println(s"EXTRACTING $uri to $out")
      try {
        val entry = archive.getEntry(path) // recently checked, shouldn't be null
        val in = archive.getInputStream(entry)
        Files.copy(in, out, REPLACE_EXISTING)
        out.toString
      } finally {
        archive.close()
      }
    }
  }

  override def getWorkspaceService(): WorkspaceService = new WorkspaceService {
    // ignore client notifications, ensime does it's own monitoring
    def didChangeConfiguration(p: DidChangeConfigurationParams): Unit = ()
    def didChangeWatchedFiles(p: DidChangeWatchedFilesParams): Unit = ()

    override def executeCommand(params: ExecuteCommandParams): CompletableFuture[Object] = params.getCommand match {
      case "ensime.import" => async {
        val args = params.getArguments.asScala.map(_.toString)
        val uri = args(0).stripPrefix("\"").stripSuffix("\"")
        val pos = new Position(args(1).toInt, args(2).toInt)
        val f = uriToFile_(uri)

        val token = tokenAtPoint(openFiles(f), pos)
        if (token.isEmpty) throw QuietExit("no token there")

        val output = ensime("search", f, token, false)
        val results = output.split("\n").toList
        System.err.println(results)

        // if there is only one result we could apply it without the
        // roundtrip, but at least this requires the user to confirm the
        // action in a consistent way.
        val question = new ShowMessageRequestParams
        question.setMessage("Import as")
        question.setType(MessageType.Info)
        question.setActions(results.map(new MessageActionItem(_)).asJava)

        client.showMessageRequest(question).thenApply { choice =>
          val content = openFiles(f).split("\n")

          val pkg = content.indexWhere(_.startsWith("package "))
          val imports = content.indexWhere(_.startsWith("import "))

          // could be more pedantic about where we put the import, but it's a
          // lot simpler to just require the user to organise their imports
          // regularly (or automatically).
          val insert = if (imports > 0) imports else pkg + 1

          val p = new Position(insert, 0)
          val r = new Range(p, p)

          val edit = new WorkspaceEdit()
          edit.setChanges(Map(uri -> List(new TextEdit(r, s"import ${choice.getTitle}\n")).asJava).asJava)
          client.applyEdit(new ApplyWorkspaceEditParams(edit, "ensime.import"))
        }

        null
      }

      case _ => null
    }

  }

  override def getNotebookDocumentService(): NotebookDocumentService = new NotebookDocumentService {
    override def didChange(p: DidChangeNotebookDocumentParams): Unit = ()
    override def didClose(p: DidCloseNotebookDocumentParams): Unit = ()
    override def didOpen(p: DidOpenNotebookDocumentParams): Unit = ()
    override def didSave(p: DidSaveNotebookDocumentParams): Unit = ()
  }

  override def shutdown(): CompletableFuture[Object] = async { null }
  override def exit(): Unit = sys.exit(0)

  // recommended by
  // https://github.com/eclipse/lsp4j/blob/main/documentation/README.md
  override def connect(client: LanguageClient): Unit = {
    this.client = client
  }
  @volatile private var client: LanguageClient = _
}
