import sbt.*

object Projects {
  val frontendsRoot = file("joern-cli/frontends")

  lazy val joerncli          = project.in(file("joern-cli"))
  lazy val querydb           = project.in(file("querydb"))
  lazy val console           = project.in(file("console"))
  lazy val dataflowengineoss = project.in(file("dataflowengineoss"))
  lazy val macros            = project.in(file("macros"))
  lazy val semanticcpg       = project.in(file("semanticcpg"))
  lazy val benchmarks        = project.in(file("benchmarks"))

  lazy val x2cpg         = project.in(frontendsRoot / "x2cpg")
  lazy val gosrc2cpg     = project.in(frontendsRoot / "gosrc2cpg")

}
