/**
 * Created by Swaneet on 31.03.14.
 */
object A2 {
  abstract class Abstract {
    val name:String
    var i:Int
    def nc = name apply i
    def f[T,R](i:T):R
    override def toString:String = "(" + name + "," + i + "," + (f()) + ")"
  }


  class Concrete extends Abstract{
    val ha:Boolean = false
    override val name = "blubb"
    override var i:Int = 15
    override def f[T,R](i:T):R = f()
    override def toString:String = "Concrete" + super.toString()
  }

}
