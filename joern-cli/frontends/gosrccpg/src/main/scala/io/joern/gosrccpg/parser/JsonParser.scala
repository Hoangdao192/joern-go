package io.joern.gosrccpg.parser

import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import io.joern.gosrccpg.ast.nodes.FileNode
import com.google.gson.{Gson, JsonDeserializer, JsonElement, JsonParseException, JsonSerializer}
import io.shiftleft.utils.IOUtils

import java.nio.file.Paths

/**
 * Load go ast from json to Node
 */
class JsonParser {

  private val gson = new Gson()
  private val objectMapper = new ObjectMapper()
  objectMapper.registerModule(DefaultScalaModule)
  objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)

  def parse(filepath: String): FileNode = {
    val fileContent: String = IOUtils.readEntireFile(Paths.get(filepath))
//    val fileNode: FileNode = gson.fromJson(fileContent, classOf[FileNode])
    val fileNode: FileNode = objectMapper.readValue(fileContent, classOf[FileNode])
    fileNode
  }

}
