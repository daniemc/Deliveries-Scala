package co.com.s4n.deliveries.domain.services

import java.util.regex.Pattern

import co.com.s4n.deliveries.domain.VO.{Move}
import co.com.s4n.deliveries.domain.entities._
import co.com.s4n.deliveries.infrastructure.{DeliveriesExecutor, FileAccess}

import scala.concurrent.{Future}
import scala.util.{Failure, Success, Try}

sealed trait DroneAlgebra {
  def defaultDrone: Drone
  def prepareDrone(name: String): Try[Drone]
  def deliverOrder(address: List[Move], drone: Drone, cityMap: MapLimits): Try[Drone]
  def makeDeliveries(drone: Drone, delivery: Delivery, cityMap: MapLimits): Try[Drone]
  def multiDroneDelivery(deliveriesList: List[String]): List[Future[Drone]]
  def getDroneNameFromFile(file: String): String
  def reportError(droneName: String, message: String): Unit
}

sealed trait DroneIntepretation extends DroneAlgebra {
  override def defaultDrone: Drone = new Drone("", PositionService.defaultPosition)

  override def prepareDrone(name: String) : Try[Drone] = Try(new Drone(name, PositionService.defaultPosition))

  override def deliverOrder(address: List[Move], drone: Drone, cityMap: MapLimits): Try[Drone] = {
    Try(address.foldLeft(drone) { (deliveryDrone, move) =>
        PositionService.reposition(move, deliveryDrone, cityMap).get
      })
  }

  override def makeDeliveries(initDrone: Drone, delivery: Delivery, cityMap: MapLimits): Try[Drone] = {
    delivery.route.foldLeft(Try(initDrone)){ (dacc, address) =>
      dacc.flatMap(deliveryDrone => deliverOrder(address.movesToGO, deliveryDrone, cityMap))
        .map(deliver => FileAccess.report(deliver))
    }
  }

  override def multiDroneDelivery(deliveriesList: List[String]): List[Future[Drone]] = {
    val drones = deliveriesList.map { deliveryFileName =>
      val exCont = DeliveriesExecutor.buildExecutor(20)
      val initDrone = DroneService.prepareDrone(getDroneNameFromFile(deliveryFileName))
      val deliveries = FileAccess.getDelivery(deliveryFileName, 3)
      val drone = Future { initDrone
        .flatMap(drone => deliveries
          .flatMap(delivery => DroneService
            .makeDeliveries(drone, delivery, MapLimitsService.defaultMap))).get
        }{ exCont }
      drone
    }
    drones
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
