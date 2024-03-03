package io.joern.gosrc2cpg.ast

import io.joern.gosrc2cpg.Config

import java.io.{BufferedReader, File, FileReader}
import java.nio.file.Files
import scala.collection.mutable.ListBuffer

class GoModule(config: Config) {

    private var _moduleName: String = ""
    private var _modulePath: String = ""

    loadModule()

    private def loadModule(): Unit = {
        val inputPath = config.inputPath
        val projectDir = File(inputPath)
        val traverse = ListBuffer[File]()
        traverse.addAll(projectDir.listFiles())
        var isContinue = true;
        while (isContinue) {
            val file = traverse.head
            if (file.isDirectory) {
                traverse.addAll(file.listFiles())
            } else if (file.isFile && file.getName.equals("go.mod")) {
                _moduleName = parseGoModFile(file)
                _modulePath = file.getParentFile.getAbsolutePath
            }
            if (traverse.nonEmpty) {
                traverse.remove(0)
            }
            if (traverse.isEmpty) {
                isContinue = false
            }
        }
    }

    private def parseGoModFile(file: File): String = {
        val fileReader = FileReader(file)
        val buffer = BufferedReader(fileReader)
        var moduleName = ""
        buffer.lines().forEach(line => {
            if (line.startsWith("module")) {
                moduleName = line.substring(
                    line.indexOf("module") + 7
                ).trim
            }
        })
        moduleName
    }

    def moduleName: String = _moduleName
    
    def modulePath: String = _modulePath

    def getGoVersion: String = {
        return ""
    }

    def getModuleDependencies: List[String] = {
        return List()
    }


}
