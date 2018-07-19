package co.com.s4n.deliveries

import co.com.s4n.deliveries.domain.VO.{A, L, R}
import co.com.s4n.deliveries.domain.entities.{Address, Delivery}
import co.com.s4n.deliveries.infrastructure.FileAccess
import org.scalatest._

import scala.util.{Success, Try}

class DeliveryTest extends FunSuite {
  test("can build success moves") {
    val moveA = FileAccess.buildMoves('A')
    val moveR = FileAccess.buildMoves('R')
    val moveL = FileAccess.buildMoves('L')
    assert(Success(A()) == moveA)
    assert(Success(R()) == moveR)
    assert(Success(L()) == moveL)
  }

  test("if an invalid move is given must return failed move") {
    val invalidMove = FileAccess.buildMoves('f')
    assert(invalidMove.isFailure)
  }

  test("can build move list") {
    val address = "AALAR"
    val deliveryRoute = FileAccess.getMoveList(address)

    assert(deliveryRoute.isSuccess)
    deliveryRoute.map(route => {
      assert(route.length > 0)
      assert(List(A(), A(), L(), A(), R()) == route)
    })
  }

  test("can't build move list with bad moves") {
    val address = "AAoAR"
    val deliveryRoute = FileAccess.getMoveList(address)
    assert(deliveryRoute.isFailure)
  }

  test("can build an Address list") {
    val stringList = List("AALA", "RAA", "RALA")
    val address = FileAccess.getAddressList(stringList, 3)

    assert(address.isSuccess)
    address.map(add => {
      assert(add.length > 0)
      assert(
        List(
          Address(List(A(), A(), L(), A())),
          Address(List(R(), A(), A())),
          Address(List(R(), A(), L(), A()))
        )
        == add
      )
    })
  }

  test("can't build an Address list with bad moves") {
    val stringList = List("AALA", "RoA", "RALA")
    val address = FileAccess.getAddressList(stringList, 3)
    assert(address.isFailure)
  }

  test("can get a delivery from a file") {
    val fileName = "in.txt"
    val delivery = FileAccess.getDelivery(fileName, 3)
    assert(delivery.isSuccess)
    delivery.map(del => {
      assert(
        Delivery(
          List(
            Address(List(A(), A(), A(), A(), L(), A(), A(), R())),
            Address(List(R(), R(), A(), L(), A(), R())),
            Address(List(A(), A(), L(), A(), R(), A(), R())),
            Address(List(A(), A(), A(), A(), L(), A(), A(), R())),
            Address(List(R(), R(), A(), L(), A(), R())),
            Address(List(A(), A(), L(), A(), R(), A(), R())))
        ) == del
      )
    })
  }

  test("can't get a delivery from a file with a bad move inside") {
    val fileName = "bad.txt"
    val delivery = FileAccess.getDelivery(fileName, 3)
    assert(delivery.isFailure)
  }



}
