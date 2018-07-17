package co.com.s4n.deliveries

import co.com.s4n.deliveries.domain.VO.{A, D, L, R}
import co.com.s4n.deliveries.domain.services.DeliveryService
import org.scalatest._

import scala.util.{Success, Try}

class DeliveryTest extends FunSuite {

  test("can build success moves") {
    val moveA = DeliveryService.buildMoves('A')
    val moveR = DeliveryService.buildMoves('R')
    val moveL = DeliveryService.buildMoves('L')
    assert(Success(A()) == moveA)
    assert(Success(R()) == moveR)
    assert(Success(L()) == moveL)
  }

  test("if an invalid move is given must return failed move") {
    val invalidMove = DeliveryService.buildMoves('f')
    assert(invalidMove.isFailure)
  }

  test("can build delivery route") {
    val address = "AALAR"
    val deliveryRoute = DeliveryService.buildDeliveryRoute(address)

    assert(6 == deliveryRoute.length)
    assert(List(A(), A(), L(), A(), R(), D()) == deliveryRoute)
  }

  test("the last position of delivery route must be type Success(D) (Delivery)") {
    val address = "AALAR"
    val deliveryRoute = DeliveryService.buildDeliveryRoute(address)
    assert(Success(D()) == deliveryRoute.last)
  }

  test("if bad instruction is given, the route returns that position in failure") {
    val address = "AgL"
    val deliveryRoute = DeliveryService.buildDeliveryRoute(address)
    assert(deliveryRoute(1).isFailure)
  }

  test("can add delivery point D") {
    val deliveryPoint = DeliveryService.addDeliveryPoint
    assert(Success(D()) == deliveryPoint)
  }

  test("can prepare delivery") {
    val delivery = Try(List("ALR", "LRA"))
    val deliveries = DeliveryService.prepareDelivery(delivery, 3)
    assert(0 < deliveries.route.length)
  }

}
