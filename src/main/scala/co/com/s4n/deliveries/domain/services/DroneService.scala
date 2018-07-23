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
  def getDroneNameFromFile(file: String): Try[String]
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
      val deliveries = FileAccess.getDelivery(deliveryFileName, 3)
      val initDrone = getDroneNameFromFile(deliveryFileName)
        .map(name => DroneService.prepareDrone(name))
        .map(drone => Future {
          drone.flatMap(drone => deliveries
            .flatMap(delivery => DroneService
              .makeDeliveries(drone, delivery, MapLimitsService.defaultMap))).get
        }{ exCont } ).get

      initDrone
    }
    drones
  }

  override def getDroneNameFromFile(file: String): Try[String] = {
    Try {
      val partName = file.split(Pattern.quote("."))
      val name = partName(0).substring(2)
      name
    }
  }

}

object DroneService extends DroneIntepretation
