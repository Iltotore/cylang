import mill._, scalalib._

object main extends ScalaModule {

  def scalaVersion = "3.1.0"

  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-parser-combinators:2.1.0"
  )

  object test extends Tests {

    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest:0.7.10"
    )

    def testFramework = "utest.runner.Framework"
  }
}