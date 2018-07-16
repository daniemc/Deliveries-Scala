package co.com.s4n.deliveries.domain.services

import co.com.s4n.deliveries.domain.VO._
import co.com.s4n.deliveries.domain.entities._
import co.com.s4n.deliveries.infrastructure.FileAccess

import scala.util.{Failure, Success, Try}

object DeliveryService {

  def getDelivery(deliveryFileName: String): Delivery ={
    val deliveryFileContent = Try(FileAccess.read(deliveryFileName))
    DeliveryService.prepareDelivery(deliveryFileContent, 10)
  }

  def prepareDelivery(delivery: Try[List[String]], deliveriesNumber: Int) : Delivery = {
    delivery match {
      case Success(deliver) => new Delivery(deliver.take(deliveriesNumber)
        .flatMap(address => buildDeliveryRoute(address)))
      case Failure(err) => new Delivery(List())
    }
  }

  def buildDeliveryRoute(address: String) = {
    address.map(move => buildMoves(move)) :+(addDeliveryPoint)
  }

  def buildMoves(move: Char): Try[Moves] = {
    move match {
      case 'A' => Success(A())
      case 'L' => Success(L())
      case 'R' => Success(R())
      case _ => Failure(new Exception(s"Move($move) not valid"))
    }
  }

  def addDeliveryPoint: Try[Moves] = {
    Success(D())
  }
}
