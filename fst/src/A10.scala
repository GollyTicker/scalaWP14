import scala.util.Try

/**
 * Created by Swaneet on 27.05.2014.
 */
object A10 {

  case class Share(shareId: String, curPrice: Double)

  case class Order(shareId: String, price: Double, numOfShares: Int)

  class Broker {
    def shareInfo(shareId: String): Share =
      if (shareId == "SAP") Share("SAP", 500.00) else throw new Exception("shareInfo")

    def contract(order: Order): Order =
      if (order.shareId == "SAP" && order.price >= 500) order
      else throw new Exception("contract")
  }

  def connectToBroker(uri: String): Broker =
    if (uri != "") new Broker else throw new Exception("connectToBroker")

  def makeOrder(share: Share, priceLimit: Double): Order =
    if (share.curPrice <= priceLimit)
      Order("SAP", priceLimit, 100)
    else
      throw new Exception("makeOrder")

  def buyShares(brokerUri: String, shareId: String): Try[Order] = {
    for {
      /* START mein Code START */
      broker <- Try { connectToBroker(brokerUri) }
      share <- Try { broker.shareInfo(shareId) }
      order <- Try { makeOrder(share, 500) }
    /* ENDE mein Code ENDE */
    } yield broker.contract(order)
  }

  def apply() = {
    println("Success(Order(SAP,500.0,100)): ")
    println(buyShares("uri", "SAP"))
    println("Failure(java.lang.Exception: connectToBroker): ")
    println(buyShares("", "SAP"))
    println("Failure(java.lang.Exception: shareInfo): ")
    println(buyShares("uri", "SA"))
  }

}


