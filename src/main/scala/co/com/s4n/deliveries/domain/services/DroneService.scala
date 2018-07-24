package co.com.s4n.deliveries.domain.services

import java.util.regex.Pattern

import co.com.s4n.deliveries.domain.VO.Move
import co.com.s4n.deliveries.domain.entities._
import co.com.s4n.deliveries.infrastructure.{DeliveriesExecutor, FileAccess}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

sealed trait DroneAlgebra {
  def defaultDrone: Drone
  def prepareDrone(name: String): Try[Drone]
  def deliverOrder(address: List[Move], drone: Drone, cityMap: MapLimits): Try[Drone]
  def makeDeliveries(drone: Drone, delivery: Delivery, cityMap: MapLimits): Try[Drone]
  def multiDroneDelivery(deliveriesList: List[String], callBack: String => Try[Delivery], exCont: ExecutionContext): List[Future[Drone]]
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
        .map(deliver => Drone.reportDeliver(deliver))
    }
  }

  override def multiDroneDelivery(deliveriesList: List[String], deliveries: String => Try[Delivery], exCont: ExecutionContext): List[Future[Drone]] = {
    deliveriesList.map { deliveryFileName =>
      getDroneNameFromFile(deliveryFileName)
        .map(name => DroneService.prepareDrone(name))
        .map(drone => Future {
          drone.flatMap(drone => deliveries(deliveryFileName)
            .flatMap(delivery => DroneService
              .makeDeliveries(drone, delivery, MapLimitsService.defaultMap))).get
        }{ exCont } ).get
    }
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
