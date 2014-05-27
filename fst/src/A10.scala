
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
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

  // Sequentiell 1
  def buySharesSeq1(brokerUri: String, shareId: String): Try[Order] = {
    for {
    /* START mein Code START */
      broker <- Try(connectToBroker(brokerUri))
      share <- Try(broker.shareInfo(shareId))
      order <- Try(makeOrder(share, share.curPrice * 1.0))
    /* ENDE mein Code ENDE */
    } yield broker.contract(order)
  }

  // Broker Cont
  def buySharesSeq2(brokerUri: String, shareIds: String*): Try[Seq[Try[Order]]] = {
    /* START mein Code START */
    for {
      broker <- Try(connectToBroker(brokerUri))
      results = for {shareId <- shareIds;
                     res = for {
                       share <- Try(broker.shareInfo(shareId))
                       order <- Try(makeOrder(share, share.curPrice * 1.0))
                     } yield broker.contract(order)
      } yield res // result eines einzigen Versuchs
    } yield results // result aller Versuche ist selber auhc ein Try. (Weil das Connect failen könnte.)
    /* ENDE mein Code ENDE */
  }

  // Broker Cont (Try kürzer)     // funzt nicht .....
  def buySharesSeq3(brokerUri: String, shareIds: String*): Try[Seq[Try[Order]]] = {
    /* START mein Code START */
    for {
      broker <- Try(connectToBroker(brokerUri))
      results = for {shareId <- shareIds;
                     res = { val share = broker.shareInfo(shareId)
                             Try(broker.contract(makeOrder(share, share.curPrice * 1.0)))
                     }
      } yield res
    } yield results
    /* ENDE mein Code ENDE */
  }

  // Broker Future
  def buySharesConc1(brokerUri: String, shareIds: String*): Future[Seq[Future[Order]]] = {
    /* START mein Code START */
    for {
      broker <- Future(connectToBroker(brokerUri))
      futures = for {shareId <- shareIds;
                     res = for {
                       share <- Future(broker.shareInfo(shareId))
                       order <- Future(makeOrder(share, share.curPrice * 1.0))
                     } yield broker.contract(order)
      } yield res // result eines einzigen Versuchs
    } yield futures
    /* ENDE mein Code ENDE */
  }

  def apply(x: Int) = x match {
    case 1 => {
      println(buySharesSeq1("uri", "SAP"))
      println(buySharesSeq1("", "SAP"))
      println(buySharesSeq1("uri", "SA"))
      // Success(Order(SAP,500.0,100))
      // Failure(java.lang.Exception: connectToBroker)
      // Failure(java.lang.Exception: shareInfo)
    }
    case 2 => {
      println(buySharesSeq2("uri", "SAP", "SQP"))
      println(buySharesSeq2("", "SAP", "SAP"))
      println(buySharesSeq2("uri", "SAP", "SAP", "SA"))
      // Success(ArrayBuffer(Success(Order(SAP,500.0,100)), Failure(java.lang.Exception: shareInfo)))
      // Failure(java.lang.Exception: connectToBroker)
      // Success(ArrayBuffer(Success(Order(SAP,500.0,100)), Success(Order(SAP,500.0,100)), Failure(java.lang.Exception: shareInfo)))
    }
    case 3 => {
      println(buySharesSeq3("uri", "SAP", "SQP"))
      println(buySharesSeq3("", "SAP", "SAP"))
      println(buySharesSeq3("uri", "SAP", "SAP", "SA"))
      // Success(ArrayBuffer(Success(Order(SAP,500.0,100)), Failure(java.lang.Exception: shareInfo)))
      // Failure(java.lang.Exception: connectToBroker)
      // Success(ArrayBuffer(Success(Order(SAP,500.0,100)), Success(Order(SAP,500.0,100)), Failure(java.lang.Exception: shareInfo)))
    }
    case 4 => {
      println(buySharesConc1("uri", "SAP", "SQP"))
      println(buySharesConc1("", "SAP", "SAP"))
      println(buySharesConc1("uri", "SAP", "SAP", "SA"))
      // Success(ArrayBuffer(Success(Order(SAP,500.0,100)), Failure(java.lang.Exception: shareInfo)))
      // Failure(java.lang.Exception: connectToBroker)
      // Success(ArrayBuffer(Success(Order(SAP,500.0,100)), Success(Order(SAP,500.0,100)), Failure(java.lang.Exception: shareInfo)))
    }
    case _ => ()
  }

}


