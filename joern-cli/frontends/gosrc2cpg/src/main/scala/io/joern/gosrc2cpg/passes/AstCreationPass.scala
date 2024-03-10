package io.joern.gosrc2cpg.passes;

import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.ast.GoModule
import io.joern.gosrc2cpg.ast.nodes.FileNode
import io.joern.gosrc2cpg.astcreation.AstCreator
import io.joern.gosrc2cpg.parser.JsonParser
import io.joern.x2cpg.{Ast, SourceFiles}
import io.joern.x2cpg.utils.{ExternalCommand, Report, TimeUtils}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewFile
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.utils.IOUtils
import org.slf4j.{Logger, LoggerFactory}

import java.io.File
import java.nio.file.Paths
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer
import scala.reflect.{ClassTag, classTag}
import scala.util.matching.Regex;
import java.util

class AstCreationPass(cpg: Cpg, config: Config, workingDir: String, goModule: GoModule, report: Report = new Report())
    extends ConcurrentWriterCpgPass[Array[String]](cpg) {

    private val sourceFileExtension: Set[String] = Set(".go")
    private val DefaultIgnoredFolders: List[Regex] = List()
    private val jsonParser: JsonParser = new JsonParser()
    private val usedTypes: util.Set[String] = new util.HashSet[String]()
    private val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

    override def generateParts(): Array[Array[String]] = {

        val binary = "/home/hoangdao/Workspace/Scala/Joern/joern-cli/frontends/gosrc2cpg/bin/go-parser/goparser"
        val command: String = s"$binary ${config.inputPath} $workingDir"
        ExternalCommand.run(command, ".")

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

//        Seq(workingDir).toArray
            val arr = collect.map(file => file.getAbsolutePath).toArray(classTag[String])
            Seq(arr).toArray
    }

    override def runOnPart(builder: DiffGraphBuilder, fileNames: Array[String]): Unit = {
        fileNames.foreach(fileName => {
            logger.info(s"Parsing $fileName")
            val path = Paths.get(fileName).toAbsolutePath;
            var relPath = SourceFiles.toRelativePath(path.toString, config.inputPath)
            //    val fileLOC = IOUtils.readLinesInFile(path).size
            val (gotCpg, duration) = TimeUtils.time {

                //            val rootFile = new File(fileName)
                //            val traverse: ListBuffer[File] = ListBuffer(rootFile)
                //            val collect: ListBuffer[File] = ListBuffer()
                //            while (traverse.nonEmpty) {
                //                val current = traverse.head
                //                if (current.isDirectory) {
                //                    traverse.addAll(current.listFiles())
                //                } else if (current.isFile && current.getName.endsWith(".json")) {
                //                    collect.addOne(current)
                //                }
                //                traverse.remove(0)
                //            }

                val parsedFile: FileNode = jsonParser.parse(fileName)
                val localDiff = new AstCreator(
                    parsedFile, fileName, goModule, usedTypes
                )(config.schemaValidation).createAst()
                builder.absorb(localDiff)
            }
        })

    }

    def getUsedPrimitiveType() = usedTypes
    
//    private def walkProjectTreeAndCreateAst(root: String): Ast = {
//        Ast().
//    }

}
