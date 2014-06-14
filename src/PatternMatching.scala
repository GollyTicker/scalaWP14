/**
 * Created by Swaneet on 31.03.14.
 */
object PatternMatching {

  def simpleTest(x:Any):Unit = x match {
    case 0 => println("0");
    case d:Double => println("got a Double")
    case 1|2|4 => println("1 two or four")
    case _ => println("UMO")
  }

  def palindrome(str:String):Boolean = str.reverse == str
}
