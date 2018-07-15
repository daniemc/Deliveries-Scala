package co.com.s4n.deliveries.domain.services

import co.com.s4n.deliveries.domain.VO.{Moves, N}
import co.com.s4n.deliveries.domain.entities._
import co.com.s4n.deliveries.infrastructure.FileAccess

import scala.util.{Failure, Success, Try}

object DroneService {
  def defaultDrone: Drone = {
    val position = new Position(0, 0, N())
    new Drone("", "", "", position)
  }

  def input(name: String): String = {
    s"in${name}.txt"
  }

  def output(name: String): String ={
    s"out${name}.txt"
  }

  def prepareDrone(name: String) : Try[Drone] = {
    val position = new Position(0, 0, N())
    Try(new Drone(name, input(name), output(name), position))
  }

  def move(movement: Try[Moves], drone: Try[Drone], cityMap: MapLimits): Try[Drone] = {

    movement match {
      case Success(step) => {
        val newPosition = PositionService.reposition(step, drone.map(_.position), cityMap)

        newPosition match {
          case Success(newPoss) => drone.map(dr => new Drone(dr.name, dr.input, dr.input, newPoss))
          case Failure(err) => {
            reportError(drone.get.name, err.getMessage)
            Failure(new Exception(err.getMessage))
          }
        }

      }
      case Failure(err) => {
        reportError(drone.get.name, err.getMessage)
        Failure(new Exception(err.getMessage))
      }
    }
  }

  def makeDeliveries(drone: Try[Drone], delivery: Delivery, cityMap: MapLimits) = {
    var deliveryDrone = drone
    delivery.route.map(action => deliveryDrone = DroneService.move(action, deliveryDrone, cityMap))
  }

  def delivery(position: Position): Position = {
    val message = s"Delivery: (${position.x}, ${position.y} - ${position.o})"
    FileAccess.write("out.txt", message)
    position
  }

  def reportError(droneName: String, message: String) = {
    FileAccess.write("ErrorsReport.txt", s"[Error] Drone ${droneName} says: ${message}")
  }
}
