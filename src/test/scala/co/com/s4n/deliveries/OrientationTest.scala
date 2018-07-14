package co.com.s4n.deliveries
import co.com.s4n.deliveries.domain.VO.{E, N, O, S}
import co.com.s4n.deliveries.domain.entities.Position
import co.com.s4n.deliveries.domain.services.OrientationService
import org.scalatest._

class OrientationTest extends FunSuite {

  val positionN = new Position(0, 0, N())
  val positionO = new Position(0, 0, O())
  val positionS = new Position(0, 0, S())
  val positionE = new Position(0, 0, E())

  test("giving a position, it must return the next position to the right") {
    val newPosition1 = OrientationService.rFrom(positionN)
    val newPosition2 = OrientationService.rFrom(positionO)
    val newPosition3 = OrientationService.rFrom(positionS)
    val newPosition4 = OrientationService.rFrom(positionE)

    assert(E() == newPosition1.o)
    assert(N() == newPosition2.o)
    assert(O() == newPosition3.o)
    assert(S() == newPosition4.o)
  }

  test("giving a position, it must return the next position to the left") {
    val newPosition1 = OrientationService.lFrom(positionN)
    val newPosition2 = OrientationService.lFrom(positionO)
    val newPosition3 = OrientationService.lFrom(positionS)
    val newPosition4 = OrientationService.lFrom(positionE)

    assert(O() == newPosition1.o)
    assert(S() == newPosition2.o)
    assert(E() == newPosition3.o)
    assert(N() == newPosition4.o)
  }
}
