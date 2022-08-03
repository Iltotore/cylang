import mill._, scalalib._, define.Source

import $file.fix, fix.FixedScalaNativeModule

def projectVersion = "0.1.0"

object main extends ScalaModule {

  def scalaVersion = "3.1.0"

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
}

object cli extends FixedScalaNativeModule {

  def scalaVersion = "3.1.0"

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