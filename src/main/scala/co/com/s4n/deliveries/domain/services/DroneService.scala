package co.com.s4n.deliveries.domain.services

import java.util.concurrent.Executors
import java.util.regex.Pattern

import co.com.s4n.deliveries.domain.VO.{Moves, N}
import co.com.s4n.deliveries.domain.entities._
import co.com.s4n.deliveries.infrastructure.FileAccess

import scala.concurrent.{ExecutionContext, Future}
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
        val newPosition = PositionService.reposition(step, drone.map(_.position), cityMap, drone.get.output)
        newPosition match {
          case Success(newPoss) => drone.map(dr => new Drone(dr.name, dr.input, dr.output, newPoss))
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

  def delivery(position: Position, name: String): Position = {
    val message = s"Delivery: (${position.x}, ${position.y} - ${position.o})"
    FileAccess.write(name, message)
    position
  }

  def multiDroneDelivery(deliveriesList: List[String]) = {
    deliveriesList.map(delivery => {
      val deliveryFile = Try(FileAccess.read(delivery))
      val mapLimit = new MapLimits(10, 10, -10, -10)
      val del = DeliveryService.prepareDelivery(deliveryFile, 10)
      val name = getDroneNameFromFile(delivery)
      val drone = prepareDrone(name)
      val exCont = ExCont
      Future {
        DroneService.makeDeliveries(drone, del, mapLimit)
      }{exCont}
    })
  }

  def ExCont: ExecutionContext = {
    implicit val exCont = new ExecutionContext {
      val threadPool = Executors.newFixedThreadPool(20)
      def execute(runnable: Runnable) {
        threadPool.submit(runnable)
      }
      def reportFailure(t: Throwable) {}
    }
    exCont
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
