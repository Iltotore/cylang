import mill._, scalalib._, scalajslib._, define.Source

import $file.fix, fix.FixedScalaNativeModule

def projectVersion = "0.1.0"

object main extends ScalaModule {

  def scalaVersion = "3.1.3"

  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-parser-combinators::2.1.1"
  )

  def testAll() = T.command {
    T.sequence {
      Seq(jvm.test.test(), native.test.test())
    }
  }

  object test extends Tests {

    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.7.11"
    )

    def testFramework = "utest.runner.Framework"
  }

  trait ChildModule extends ScalaModule {

    def scalaVersion = main.scalaVersion

    def ivyDeps = super.ivyDeps() ++ main.ivyDeps()

    def sources = T.sources(
      super.sources() ++ main.sources()
    )

    def resources = T.sources(
      super.resources() ++ main.resources()
    )
  }

  trait ChildTestModule extends TestModule {

    def ivyDeps = super.ivyDeps() ++ main.test.ivyDeps()

    def testFramework = main.test.testFramework

    def sources = T.sources(
      super.sources() ++ main.test.sources()
    )

    def resources = T.sources(
      super.resources() ++ main.test.resources()
    )
  }

  object jvm extends ChildModule {

    object test extends Tests with ChildTestModule
  }

  object native extends ChildModule with FixedScalaNativeModule {

    def scalaNativeVersion = "0.4.4"

    //TODO submit changes to Mill
    def scalaLibraryIvyDeps = T {
      Agg(
        ivy"${scalaOrganization()}::scala3-library:${scalaVersion()}".forceVersion()
      )
    }

    object test extends Tests with ChildTestModule

  }

  object js extends ChildModule with ScalaJSModule {

    def scalaJSVersion = "1.11.0"
  }
}

object cli extends FixedScalaNativeModule {

  def scalaVersion = main.scalaVersion

  def scalaNativeVersion = "0.4.4"

  def moduleDeps = Seq(main.native)

  def versionFile: Source = T.source {
    val file = T.dest / "versions.properties"
    os.write(
      file,
      Map(
        "CYLang" -> projectVersion,
        "Scala" -> scalaVersion(),
        "Scala Native" -> scalaNativeVersion()
      )
        .map {case (a, b) => s"$a=$b"}
        .mkString("\n")
    )
    T.dest
  }

  def resources = T.sources { super.resources() :+ versionFile() }
}

object webeditor extends ScalaJSModule {

  def scalaVersion = main.scalaVersion

  def scalaJSVersion = "1.11.0"

  def moduleDeps = Seq(main.js)

  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"org.scala-js::scalajs-dom::2.2.0",
    ivy"io.indigoengine::tyrian::0.5.1",
    ivy"io.indigoengine::tyrian-io::0.5.1"
  )

  def moduleKind = T(mill.scalajslib.api.ModuleKind.CommonJSModule)

  def buildSite() = T.command {
    os.copy.into(fastOpt().path, T.dest)

    val resourcesDir = T.dest / "resources" //os.pwd / "webeditor" / "dist"

    if(!os.exists(resourcesDir)) os.makeDir(resourcesDir)
    
    (resources() ++ main.js.resources())
      .map(_.path)
      .filter(os.exists)
      .flatMap(os.list)
      .foreach(x => os.copy.into(x, resourcesDir))

  }
}
