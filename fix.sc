import mill.T

import $ivy.`org.scala-native::tools:0.4.4`

import mill.scalanativelib.ScalaNativeModule
import mill.scalanativelib.api.{LTO, NativeConfig, NativeLogLevel, ReleaseMode}

import java.lang.System.{err, out}
import scala.scalanative.util.Scope
import scala.scalanative.build.{Build, BuildException, Config, Discover, GC, Logger, Mode, LTO => ScalaNativeLTO, NativeConfig => ScalaNativeNativeConfig}
import scala.scalanative.nir.Versions
import mill.scalanativelib.api.{GetFrameworkResult, LTO, NativeConfig, NativeLogLevel, ReleaseMode}

import scala.jdk.OptionConverters.RichOption

/*
Temporary fix for Mill not opening `embedResources` option for ScalaNative. Should be PR-ed.
 */


def logger(level: NativeLogLevel) =
  Logger(
    traceFn = msg => if (level.value >= NativeLogLevel.Trace.value) err.println(s"[trace] $msg"),
    debugFn = msg => if (level.value >= NativeLogLevel.Debug.value) out.println(s"[debug] $msg"),
    infoFn = msg => if (level.value >= NativeLogLevel.Info.value) out.println(s"[info] $msg"),
    warnFn = msg => if (level.value >= NativeLogLevel.Warn.value) out.println(s"[warn] $msg"),
    errorFn = msg => if (level.value >= NativeLogLevel.Error.value) err.println(s"[error] $msg")
  )


def config(
            mainClass: String,
            classpath: Array[java.io.File],
            nativeWorkdir: java.io.File,
            nativeClang: java.io.File,
            nativeClangPP: java.io.File,
            nativeTarget: java.util.Optional[String],
            nativeCompileOptions: Array[String],
            nativeLinkingOptions: Array[String],
            nativeGC: String,
            nativeLinkStubs: Boolean,
            nativeLTO: LTO,
            releaseMode: ReleaseMode,
            nativeOptimize: Boolean,
            logLevel: NativeLogLevel,
            embedResources: Boolean
          ): NativeConfig = {
  val entry = Versions.current match {
    case "0.4.0" | "0.4.1" | "0.4.2" => mainClass + "$"
    case _ => mainClass
  }
  val config =
    Config.empty
      .withMainClass(entry)
      .withClassPath(classpath.map(_.toPath))
      .withWorkdir(nativeWorkdir.toPath)
      .withCompilerConfig(
        ScalaNativeNativeConfig.empty
          .withClang(nativeClang.toPath)
          .withClangPP(nativeClangPP.toPath)
          .withTargetTriple(if (nativeTarget.isPresent) Some(nativeTarget.get) else None)
          .withCompileOptions(nativeCompileOptions)
          .withLinkingOptions(nativeLinkingOptions)
          .withGC(GC(nativeGC))
          .withLinkStubs(nativeLinkStubs)
          .withMode(Mode(releaseMode.value))
          .withOptimize(nativeOptimize)
          .withLTO(ScalaNativeLTO(nativeLTO.value))
          .withEmbedResources(embedResources)
      )
      .withLogger(logger(logLevel))
  new NativeConfig(config)
}

def nativeLinkWorker(nativeConfig: NativeConfig, outPath: java.io.File): java.io.File = {
  val config = nativeConfig.config.asInstanceOf[Config]
  Build.build(config, outPath.toPath)(Scope.unsafe())
  outPath
}

trait FixedScalaNativeModule extends ScalaNativeModule {

  def nativeConfig = T.task {
    val classpath = runClasspath().map(_.path).filter(_.toIO.exists).toList

    config(
      finalMainClass(),
      classpath.toArray.map(_.toIO),
      nativeWorkdir().toIO,
      nativeClang().toIO,
      nativeClangPP().toIO,
      nativeTarget().toJava,
      nativeCompileOptions(),
      nativeLinkingOptions(),
      nativeGC(),
      nativeLinkStubs(),
      nativeLTO(),
      releaseMode(),
      nativeOptimize(),
      logLevel(),
      embedResources = true
    )
  }

  // Generates native binary
  def nativeLink = T {
    os.Path(nativeLinkWorker(nativeConfig(), (T.dest / "out").toIO))
  }
}