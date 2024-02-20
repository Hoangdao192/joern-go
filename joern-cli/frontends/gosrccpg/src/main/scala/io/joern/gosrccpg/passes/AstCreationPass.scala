package io.joern.gosrccpg.passes;

import io.joern.gosrccpg.Config
import io.joern.gosrccpg.ast.nodes.FileNode
import io.joern.gosrccpg.astcreation.AstCreator
import io.joern.gosrccpg.parser.JsonParser
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.utils.{ExternalCommand, Report, TimeUtils}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.utils.IOUtils

import java.io.File
import java.nio.file.Paths
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer
import scala.reflect.{ClassTag, classTag}
import scala.util.matching.Regex;

class AstCreationPass(cpg:Cpg, config:Config, workingDir: String, report:Report = new Report())
        extends ConcurrentWriterCpgPass[String](cpg) {

  private val sourceFileExtension: Set[String] = Set(".go")
  private val DefaultIgnoredFolders: List[Regex] = List()
  private val jsonParser: JsonParser = new JsonParser()

  override def generateParts(): Array[String] = {
    val binary = "/home/hoangdao/Workspace/Scala/Joern/joern-cli/frontends/gosrccpg/bin/go-parser/goparser"
    val command: String = s"$binary ${config.inputPath} $workingDir"
    ExternalCommand.run(command, ".")
//    val arr = SourceFiles.determine(
//      workingDir,
//      Set("go.json"),
//      Option(DefaultIgnoredFolders),
//      Option(config.ignoredFilesRegex),
//      Option(config.ignoredFiles)
//    ).toArray(classTag[String])

    val rootFile = new File(workingDir)
    val traverse: ListBuffer[File] = ListBuffer(rootFile)
    val collect: ListBuffer[File] = ListBuffer()
    while (traverse.nonEmpty) {
      val current = traverse.head
      if (current.isDirectory) {
        traverse.addAll(current.listFiles())
      } else if (current.isFile && current.getName.endsWith(".json")) {
        collect.addOne(current)
      }
      traverse.remove(0)
    }

    val arr = collect.map(file => file.getAbsolutePath).toArray(classTag[String])
    arr
//    SourceFiles
//      .determine(
//        config.inputPath,
//        sourceFileExtension,
//        Option(DefaultIgnoredFolders),
//        Option(config.ignoredFilesRegex),
//        Option(config.ignoredFiles)
//      ).toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, fileName: String): Unit = {
    val path = Paths.get(fileName).toAbsolutePath;
    var relPath = SourceFiles.toRelativePath(path.toString, config.inputPath)
    val fileLOC = IOUtils.readLinesInFile(path).size
    val (gotCpg, duration) = TimeUtils.time {
      val parsedFile: FileNode = jsonParser.parse(fileName)
      val localDiff = new AstCreator(
        parsedFile, fileName
      )(config.schemaValidation).createAst()
      builder.absorb(localDiff)
    }
  }

}
