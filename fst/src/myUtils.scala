/**
 * Created by Swaneet on 02.06.2014.
 */
object myUtils {
  def exec[T](name: String, delay: Long, block: => T): T = {
    println(s"start $name")
    Thread.sleep(delay)
    val r= block
    println(s"stop $name.")
    r
  }

  object OSEnvironment {
    import System.{getProperty => prop}

    val numCores= Runtime.getRuntime.availableProcessors
    val name= prop("os.name","unknown OS")
    val version= prop("os.version","unknown version")
    val architecture= prop("os.arch","unknown architecture")
    val jvm= prop("java.version", "unknown")

    def printOSSpecs {
      println("Cores : %d".format(numCores))
      println("OS : %s %s (%s)".format(name,version,architecture))
      println("JVM : " + jvm)
    }
  }

  object ExecTiming {
    def resultAndNanos[A](block: => A) = {
      val t = System.nanoTime
      (block,System.nanoTime - t)
    }

    def printMillis[A](block: => A) {
      val (result,nanoTime)= resultAndNanos(block)
      println("time: "+ "~%.3fms".format((nanoTime/1000L)/1000.0)+ "\n")
    }

    def printResultAndMillis[A](block: => A) {
      val (result,nanoTime)= resultAndNanos(block)
      println(result + "\ntime: "+ "~%.3fms".format((nanoTime/1000L)/1000.0)+ "\n")
    }
  }

}
