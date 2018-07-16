package co.com.s4n.deliveries.domain.services

import java.util.regex.Pattern

import co.com.s4n.deliveries.domain.VO.{Move}
import co.com.s4n.deliveries.domain.entities._
import co.com.s4n.deliveries.infrastructure.{DeliveriesExecutor, FileAccess}

import scala.concurrent.{Future}
import scala.util.{Failure, Success, Try}

sealed trait DroneAlgebra {
  def defaultDrone: Drone
  def output(name: String): String
  def prepareDrone(name: String): Try[Drone]
  def move(movement: Try[Move], drone: Try[Drone], cityMap: MapLimits): Try[Drone]
  def goToNewPosition(step: Move, drone: Try[Drone], cityMap: MapLimits): Try[Drone]
  def makeDeliveries(drone: Try[Drone], delivery: Delivery, cityMap: MapLimits): Try[Drone]
  def deliverOrder(position: Position, name: String): Position
  def multiDroneDelivery(deliveriesList: List[String]): List[Future[Try[Drone]]]
  def getDroneNameFromFile(file: String): String
  def reportError(droneName: String, message: String): Unit
}

sealed trait DroneIntepretation extends DroneAlgebra {
  override def defaultDrone: Drone = new Drone("", "", PositionService.defaultPosition)

  override def output(name: String): String =s"out${name}.txt"

  override def prepareDrone(name: String) : Try[Drone] = Try(new Drone(name, output(name), PositionService.defaultPosition))

  override def move(movement: Try[Move], drone: Try[Drone], cityMap: MapLimits): Try[Drone] = movement match {
    case Success(step) => goToNewPosition(step, drone, cityMap)
    case Failure(err) => {
      reportError(drone.get.name, err.getMessage)
      Failure(new Exception(err.getMessage))
    }
  }

  override def goToNewPosition(step: Move, drone: Try[Drone], cityMap: MapLimits) = {
    PositionService.reposition(
      step,
      drone.map(_.position),
      cityMap,
      drone.get.output) match {
      case Success(newPoss) => drone.map(dr => new Drone(dr.name, dr.output, newPoss))
      case Failure(err) => {
        reportError(drone.get.name, err.getMessage)
        Failure(new Exception(err.getMessage))
      }
    }
  }

  override def makeDeliveries(drone: Try[Drone], delivery: Delivery, cityMap: MapLimits) = {
    delivery.route.foldLeft(drone) {(deliveryDrone, action) => {
      DroneService.move(action, deliveryDrone, cityMap)
    }}
  }

  override def deliverOrder(position: Position, name: String): Position = {
    val message = s"Delivery: (${position.x}, ${position.y} - ${position.o})"
    FileAccess.write(name, message)
    position
  }

  override def multiDroneDelivery(deliveriesList: List[String]) = {
    deliveriesList.map { deliveryFileName =>
      val delivery = DeliveryService.getDelivery(deliveryFileName)
      val drone = prepareDrone(getDroneNameFromFile(deliveryFileName))
      val exCont = DeliveriesExecutor.buildExecutor(20)
      Future {
        DroneService.makeDeliveries(drone, delivery, MapLimitsService.defaultMap)
      }{ exCont }
    }
  }

  override def getDroneNameFromFile(file: String): String = {
    val partName = file.split(Pattern.quote("."))
    val name = partName(0).substring(2)
    name
  }

  override def reportError(droneName: String, message: String) = {
    FileAccess.write("ErrorsReport.txt", s"[Error] Drone ${droneName} says: ${message}")
  }
}

object DroneService extends DroneIntepretation
