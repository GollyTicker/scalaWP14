
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}
import scala.concurrent._
import ExecutionContext.Implicits.global
import myUtils.{await,exec}

/**
 * Created by Swaneet on 27.05.2014.
 */
object A10 {

  // Aufrufen mit A10(0) bis A10(2) in Console

  val delay = 1000

  case class Share(shareId: String, curPrice: Double)

  case class Order(shareId: String, price: Double, numOfShares: Int)

  class Broker {
    def shareInfo(shareId: String): Share =
      exec("shareInfo", delay, (if (shareId == "SAP") Share("SAP", 500.00) else throw new Exception("shareInfo")))

    def contract(order: Order): Order =
      exec("contract", delay, {
        if (order.shareId == "SAP" && order.price >= 500) order
        else throw new Exception("contract")
      })
  }

  def connectToBroker(uri: String): Broker =
    exec("connectToBroker", delay, (if (uri != "") new Broker else throw new Exception("connectToBroker")))

  def makeOrder(share: Share, priceLimit: Double): Order = exec("makeOrder", delay, {
    if (share.curPrice <= priceLimit)
      Order("SAP", priceLimit, 100)
    else
      throw new Exception("makeOrder")
  })

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

  // Broker Async
  def buySharesAsync1(brokerUri: String, shareIds: String*): Future[Seq[Future[Order]]] = {
    val brokerConnFut = Future(connectToBroker(brokerUri))

    val futShr: Broker => String => Future[Share] = b => shrId => Future(b.shareInfo(shrId))
    val futOrd: Share => Double => Future[Order] = shr => newPrice => Future(makeOrder(shr, newPrice))
    for {
      broker <- brokerConnFut
      results = for {shareId <- shareIds;
                     res = for {
                       share <- futShr(broker)(shareId)
                       order <- futOrd(share)(share.curPrice * 1.0)
                     } yield broker.contract(order)
      } yield res // result eines einzigen Versuchs
    } yield results
  }

  def apply(x: Int) = x match {
    case 1 => {
      println(buySharesSeq1("uri", "SAP"))
      println(buySharesSeq1("", "SAP"))
      println(buySharesSeq1("uri", "SA"))
    }
    case 2 => {
      println(buySharesSeq2("uri", "SAP", "SAP"))
      println(buySharesSeq2("", "SAP", "SAP"))
      println(buySharesSeq2("uri", "SAP", "SAP", "SA"))
    }
    case 3 => {
      val resFut:Future[Seq[Future[Order]]] = buySharesAsync1("uri", "SAP", "SAP", "SA")
      resFut map { ls =>
        ls map { x => await(x)
                         println(x.isCompleted)
                         println(x.value) }
      }
      await(resFut)
    }
    case _ => ()
  }
}


