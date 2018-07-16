package co.com.s4n.deliveries.domain.services

import java.util.regex.Pattern

import co.com.s4n.deliveries.domain.VO.{Moves, N}
import co.com.s4n.deliveries.domain.entities._
import co.com.s4n.deliveries.infrastructure.{DeliveriesExecutor, FileAccess}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object DroneService {
  def defaultDrone: Drone = {
    new Drone("", "", PositionService.defaultPosition)
  }

  def output(name: String): String ={
    s"out${name}.txt"
  }

  def prepareDrone(name: String) : Try[Drone] = {
    Try(new Drone(name, output(name), PositionService.defaultPosition))
  }

  def move(movement: Try[Moves], drone: Try[Drone], cityMap: MapLimits): Try[Drone] = {
    movement match {
      case Success(step) => goToNewPosition(step, drone, cityMap)
      case Failure(err) => {
        reportError(drone.get.name, err.getMessage)
        Failure(new Exception(err.getMessage))
      }
    }
  }

  def goToNewPosition(step: Moves, drone: Try[Drone], cityMap: MapLimits) = {

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

  def makeDeliveries(drone: Try[Drone], delivery: Delivery, cityMap: MapLimits) = {
    delivery.route.foldLeft(drone) {(deliveryDrone, action) => {
      DroneService.move(action, deliveryDrone, cityMap)
    }}
  }

  def deliverOrder(position: Position, name: String): Position = {
    val message = s"Delivery: (${position.x}, ${position.y} - ${position.o})"
    FileAccess.write(name, message)
    position
  }

  def multiDroneDelivery(deliveriesList: List[String]) = {
    deliveriesList.map(deliveryFileName => {
      val delivery = DeliveryService.getDelivery(deliveryFileName)
      val drone = prepareDrone(getDroneNameFromFile(deliveryFileName))
      val exCont = DeliveriesExecutor.buildExecutor(20)
      Future {
        DroneService.makeDeliveries(drone, delivery, MapLimitsService.defaultMap)
      }{ exCont }
    })
  }

  def getDroneNameFromFile(file: String): String = {
    val partName = file.split(Pattern.quote("."))
    val name = partName(0).substring(2)
    name
  }

  def reportError(droneName: String, message: String) = {
    FileAccess.write("ErrorsReport.txt", s"[Error] Drone ${droneName} says: ${message}")
  }
}
