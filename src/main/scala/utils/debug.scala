package utils

object debug {
  // some information can be printed for debug purpose
  val bean = java.lang.management.ManagementFactory.getRuntimeMXBean
  val isDebug = bean.getInputArguments.toString.contains("-agentlib:jdwp")

  def apply(exp: => Any): Unit = {
    if (isDebug)
      println(exp)
  }
}
