package co.com.s4n.deliveries
import co.com.s4n.deliveries.domain.VO._
import co.com.s4n.deliveries.domain.entities._
import co.com.s4n.deliveries.domain.services.{DroneService, MapLimitsService}
import co.com.s4n.deliveries.infrastructure.FileAccess
import org.scalatest._

import scala.util.{Success, Try}

class DroneTest extends FunSuite {

  test("a dron can be prepared") {
    val preparedDrone = DroneService.prepareDrone("01")
    assert(preparedDrone.isSuccess)
  }

  test("a dron can make deliveries") {
    val deliveriesList = new Delivery(
      List(
        Address(List(A(), L(), A())),
        Address(List(A(), R(), A())),
        Address(List(A(), A()))
      )
    )
    val preparedDelivery = Try(deliveriesList)
    val initDrone = DroneService.prepareDrone("33")
    val result = initDrone
      .flatMap(drone => preparedDelivery
        .flatMap(delivery => DroneService
          .makeDeliveries(drone, delivery, MapLimitsService.defaultMap)))

    assert(result.isSuccess)

    var resultName = ""
    var xPosition = 0
    var yPosition = 0
    var oPosition: Orientation = S()
    result.map {r =>
      resultName = r.name
      xPosition = r.position.x
      yPosition = r.position.y
      oPosition = r.position.o
    }

    assert("33" == resultName)
    assert(-2 == xPosition)
    assert(4 == yPosition)
    assert(N() == oPosition)
  }

  test("a dron can make deliveries from file") {
    val initDrone = DroneService.prepareDrone("30")
    val deliveries = FileAccess.getDelivery("in.txt", 3)
    val result = initDrone
      .flatMap(drone => deliveries
        .flatMap(delivery => DroneService
          .makeDeliveries(drone, delivery, MapLimitsService.defaultMap)))

    assert(result.isSuccess)

  }

  test("a dron can't make deliveries from a file with a bad move inside") {
    val initDrone = DroneService.prepareDrone("31")
    val deliveries = FileAccess.getDelivery("bad.txt", 3)
    val result = initDrone
      .flatMap(drone => deliveries
        .flatMap(delivery => DroneService
          .makeDeliveries(drone, delivery, MapLimitsService.defaultMap)))

    assert(result.isFailure)

  }

  test("a dron can't go beyond map limits") {
    val deliveries = Try(new Delivery(List(Address(List(A(), A(), A(), A(), A(), A(), A(), A(), A(), A(), A(), A())))))
    val initDrone = DroneService.prepareDrone("33")
    val result = initDrone
      .flatMap(drone => deliveries
        .flatMap(delivery => DroneService
          .makeDeliveries(drone, delivery, MapLimitsService.defaultMap)))

    assert(result.isFailure)
  }

  test("a dron can make multi deliveries from files") {
    val deliveriesList = FileAccess.list(Config.fullPath)
    val deliveriesResult = deliveriesList.map(deliveries => DroneService.multiDroneDelivery(deliveries.take(20)))

    assert(deliveriesList.isSuccess)
    assert(deliveriesList.get.length > 0)
  }
}
