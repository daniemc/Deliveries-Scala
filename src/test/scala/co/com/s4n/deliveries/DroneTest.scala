package co.com.s4n.deliveries
import co.com.s4n.deliveries.domain.VO._
import co.com.s4n.deliveries.domain.entities._
import co.com.s4n.deliveries.domain.services.{DroneService, MapLimitsService}
import co.com.s4n.deliveries.infrastructure.{DeliveriesExecutor, FileAccess}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.{time, _}
import scala.concurrent.duration._

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
    val deliveriesList = FileAccess.getDelivery("bad.txt", 3)

    val result = initDrone
      .flatMap(drone => deliveriesList
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
    val deliveryFunc = (param: String) => FileAccess.getDelivery(param, 3)
    val exCont = DeliveriesExecutor.buildExecutor(20)
    val deliveriesResult = deliveriesList
      .map(deliveries => DroneService.multiDroneDelivery(deliveries.take(20), deliveryFunc, exCont))

    val result = Future.sequence(deliveriesResult.get)

    Await.result(result, 5 seconds)

    val file1 = FileAccess.read("out01.txt")
    val file2 = FileAccess.read("out02.txt")
    val file3 = FileAccess.read("out03.txt")
    val file4 = FileAccess.read("out04.txt")

    assert(List("Delivery: (-2, 4 - N())", "Delivery: (-1, 3 - S())", "Delivery: (0, 0 - O())") == file1.get)
    assert(List("Delivery: (-2, 3 - N())", "Delivery: (-1, 4 - E())", "Delivery: (1, 6 - N())") == file2.get)
    assert(List("Delivery: (0, 4 - N())", "Delivery: (1, 5 - E())", "Delivery: (4, 4 - S())") == file3.get)
    assert(List("Delivery: (-2, 0 - N())", "Delivery: (-3, 1 - N())", "Delivery: (-4, 3 - O())") == file4.get)


    assert(deliveriesResult.isSuccess)
    assert(deliveriesResult.get.length > 0)

  }

  test("default input files must be the same always") {
    val file1 = FileAccess.read("in01.txt")
    val file2 = FileAccess.read("in02.txt")
    val file3 = FileAccess.read("in03.txt")
    val file4 = FileAccess.read("in04.txt")

    assert(List("AAAALAAR", "RRALAR", "AALARAR") == file1.get.take(3))
    assert(List("AAALAAR", "RALAR", "AALAA") == file2.get.take(3))
    assert(List("AAAA", "RALAR", "AAARA") == file3.get.take(3))
    assert(List("LAAR", "ALAR", "AALA") == file4.get.take(3))
  }
}
