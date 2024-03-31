import scala.sys.process.stringToProcess
import scala.util.Try
import versionsort.VersionHelper
import java.io.File
import com.typesafe.config.{Config, ConfigFactory}
import sbt.Keys.libraryDependencies

name := "gosrc2cpg"

dependsOn(Projects.dataflowengineoss % "compile->compile;test->test", Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "io.shiftleft"              %% "codepropertygraph" % Versions.cpg,
  "org.scalatest"             %% "scalatest"         % Versions.scalatest % Test,
  "com.lihaoyi"               %% "os-lib"            % "0.9.1",
  "com.fasterxml.jackson.core" % "jackson-databind"  % "2.15.2",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.15.2",
  "io.circe"                  %% "circe-core"        % Versions.circe,
  "io.circe"                  %% "circe-generic"     % Versions.circe,
  "io.circe"                  %% "circe-parser"      % Versions.circe,
  "com.google.code.gson"      %  "gson"             % "2.10.1",
  "org.apache.commons"        %  "commons-compress" % "1.26.1"
)

scalacOptions ++= Seq(
  "-deprecation" // Emit warning and location for usages of deprecated APIs.
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

lazy val appProperties = settingKey[Config]("App Properties")
appProperties := {
  val path            = (Compile / resourceDirectory).value / "application.conf"
  val applicationConf = ConfigFactory.parseFile(path).resolve()
  applicationConf
}

lazy val GoParser = "goparser"

lazy val goParserBinaries = taskKey[Seq[String]]("Go parser binary names")
goParserBinaries := List("goparser")

lazy val goAstGenDlTask = taskKey[Unit](s"Download Go parser binaries")
goAstGenDlTask := {
  val goParserDir = baseDirectory.value / "bin" / "go-parser"
  goParserDir.mkdirs()

  goParserBinaries.value.foreach { fileName =>
    val dest = goParserDir / fileName
    if (!dest.exists) {
      IO.copyFile(new File(goParserDir + "/" + fileName), dest)
    }
  }

  val distDir = (Universal / stagingDirectory).value / "bin" / "go-parser"
  distDir.mkdirs()
  IO.copyDirectory(goParserDir, distDir)

  // permissions are lost during the download; need to set them manually
  goParserDir.listFiles().foreach(_.setExecutable(true, false))
  distDir.listFiles().foreach(_.setExecutable(true, false))
}

Compile / compile := ((Compile / compile) dependsOn goAstGenDlTask).value

lazy val goAstGenSetAllPlatforms = taskKey[Unit](s"Set ALL_PLATFORMS")
goAstGenSetAllPlatforms := { System.setProperty("ALL_PLATFORMS", "TRUE") }

stage := Def
  .sequential(goAstGenSetAllPlatforms, Universal / stage)
  .andFinally(System.setProperty("ALL_PLATFORMS", "FALSE"))
  .value

// Also remove astgen binaries with clean, e.g., to allow for updating them.
// Sadly, we can't define the bin/ folders globally,
// as .value can only be used within a task or setting macro
//cleanFiles ++= Seq(
//  baseDirectory.value / "bin" / "go-parser",
//  (Universal / stagingDirectory).value / "bin" / "go-parser"
//) ++ goParserBinaries.value.map(fileName => SimpleCache.encodeFile(s"${goAstGenDlUrl.value}$fileName"))
