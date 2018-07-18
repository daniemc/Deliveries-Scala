package co.com.s4n.deliveries.domain.services

import co.com.s4n.deliveries.domain.VO._
import co.com.s4n.deliveries.domain.entities._
import co.com.s4n.deliveries.infrastructure.FileAccess

import scala.util.{Failure, Success, Try}

sealed trait DeliveryAlgebra {
  def getDelivery(deliveryFileName: String): Delivery
  def prepareDelivery(delivery: Try[List[String]], deliveriesNumber: Int) : Delivery
  def buildDeliveryRoute(address: String): Seq[Try[Move]]
  def buildMoves(move: Char): Try[Move]
  def addDeliveryPoint: Try[Move]
}

sealed trait DeliveryInterpreter extends DeliveryAlgebra {
  override def getDelivery(deliveryFileName: String): Delivery = {
    val deliveryFileContent = Try(FileAccess.read(deliveryFileName))
    DeliveryService.prepareDelivery(deliveryFileContent.get, 10)
  }

  override def prepareDelivery(delivery: Try[List[String]], deliveriesNumber: Int) : Delivery = delivery match {
    case Success(deliver) => new Delivery(deliver.take(deliveriesNumber)
      .flatMap(address => buildDeliveryRoute(address)))
    case Failure(err) => new Delivery(List())
  }

  override def buildDeliveryRoute(address: String) = address
    .map(move => buildMoves(move)) :+(addDeliveryPoint)

  override def buildMoves(move: Char): Try[Move] = move match {
    case 'A' => Success(A())
    case 'L' => Success(L())
    case 'R' => Success(R())
    case _ => Failure(new Exception(s"Move($move) not valid"))
  }

  override def addDeliveryPoint: Try[Move] = Success(D())
}

object DeliveryService extends DeliveryInterpreter
