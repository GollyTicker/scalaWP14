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
}
