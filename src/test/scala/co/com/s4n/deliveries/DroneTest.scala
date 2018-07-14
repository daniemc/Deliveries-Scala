package co.com.s4n.deliveries
import co.com.s4n.deliveries.domain.entities.MapLimits
import co.com.s4n.deliveries.domain.services.{DeliveryService, DroneService}
import co.com.s4n.deliveries.infrastructure.FileAccess
import org.scalatest._

import scala.util.Try

class DroneTest extends FunSuite {
  test("dron can make delivers") {
    val delivery = Try(List("ALR", "LRA"))
    val mapLimit = new MapLimits(10, 10, -10, -10)
    val deliveries = DeliveryService.prepareDelivery(delivery, 3)
    val drone = DroneService.prepareDrone("01")
    val deliveriesResult =  Try(DroneService.makeDeliveries(drone, deliveries, mapLimit))
    assert(deliveriesResult.isSuccess)
  }

  test("pass bad moves to drone will fail") {
    val delivery = Try(List("AALR", "LRAB"))
    val mapLimit = new MapLimits(10, 10, -10, -10)
    val deliveries = DeliveryService.prepareDelivery(delivery, 3)
    val drone = DroneService.prepareDrone("01")
    val deliveriesResult =  Try(DroneService.makeDeliveries(drone, deliveries, mapLimit))
    assert(deliveriesResult.isFailure)
  }

  test("go beyond map limits will fail") {
    val delivery = Try(List("ALR", "LAAAAAAAAAAAARA"))
    val mapLimit = new MapLimits(10, 10, -10, -10)
    val deliveries = DeliveryService.prepareDelivery(delivery, 3)
    val drone = DroneService.prepareDrone("01")
    val deliveriesResult =  Try(DroneService.makeDeliveries(drone, deliveries, mapLimit))
    assert(deliveriesResult.isFailure)
  }

  test("a dron can make delivers from file") {
    val file = Try(FileAccess.read("in.txt"))
    val mapLimit = new MapLimits(10, 10, -10, -10)
    val deliveries = DeliveryService.prepareDelivery(file, 3)
    val drone = DroneService.prepareDrone("01")
    val deliveriesResult =  Try(DroneService.makeDeliveries(drone, deliveries, mapLimit))
    assert(deliveriesResult.isSuccess)

  }
}
