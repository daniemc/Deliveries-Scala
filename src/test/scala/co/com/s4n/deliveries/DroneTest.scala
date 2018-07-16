package co.com.s4n.deliveries
import co.com.s4n.deliveries.domain.entities.MapLimits
import co.com.s4n.deliveries.domain.services.{DeliveryService, DroneService, MapLimitsService}
import co.com.s4n.deliveries.infrastructure.FileAccess
import org.scalatest._

import scala.util.Try

class DroneTest extends FunSuite {
  test("dron can make delivers") {
    val delivery = Try(List("ALR", "LRA"))
    val deliveries = DeliveryService.prepareDelivery(delivery, 3)
    val drone = DroneService.prepareDrone("01")
    val deliveriesResult =  Try(DroneService.makeDeliveries(drone, deliveries, MapLimitsService.defaultMap))
    assert(deliveriesResult.isSuccess)
  }

  test("pass bad moves to drone will fail") {
    val delivery = Try(List("AALR", "LRAB"))
    val deliveries = DeliveryService.prepareDelivery(delivery, 3)
    val drone = DroneService.prepareDrone("01")
    val deliveriesResult = Try(DroneService.makeDeliveries(drone, deliveries, MapLimitsService.defaultMap))
    assert(deliveriesResult.isFailure)
  }

  test("go beyond map limits will fail") {
    val delivery = Try(List("ALR", "LAAAAAAAAAAAARA"))
    val deliveries = DeliveryService.prepareDelivery(delivery, 3)
    val drone = DroneService.prepareDrone("01")
    val deliveriesResult = Try(DroneService.makeDeliveries(drone, deliveries, MapLimitsService.defaultMap))
    assert(deliveriesResult.isFailure)
  }

  test("a dron can make delivers from file") {
    val file = Try(FileAccess.read("in.txt"))
    val deliveries = DeliveryService.prepareDelivery(file, 3)
    val drone = DroneService.prepareDrone("01")
    val deliveriesResult = Try(DroneService.makeDeliveries(drone, deliveries, MapLimitsService.defaultMap))
    assert(deliveriesResult.isSuccess)

  }

  test("a dron can make multi deliveries from files") {
    val deliveriesList = FileAccess.list
    val deliveriesResult = DroneService.multiDroneDelivery(deliveriesList)
    assert(0 < deliveriesResult.length)
  }
}
