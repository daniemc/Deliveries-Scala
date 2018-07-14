package co.com.s4n.deliveries
import co.com.s4n.deliveries.domain.VO.N
import co.com.s4n.deliveries.domain.entities.{MapLimits, Position}
import co.com.s4n.deliveries.domain.services.MapLimitsService
import org.scalatest._

class MapTest extends FunSuite {

  val mapLimitToTest = new MapLimits(5, 5, -5, -5)

  test("giving a map limit, it can be validated in y axis (N, S)") {
    val CorrectPosition = new Position(2, 2, N())
    val validation1 = MapLimitsService.validateNS(CorrectPosition, mapLimitToTest)
    assert(validation1.isSuccess)

    val IncorrectPosition = new Position(2, 7, N())
    val validation2 = MapLimitsService.validateNS(IncorrectPosition, mapLimitToTest)
    assert(validation2.isFailure)
  }

  test("giving a map limit, it can be validated in y axis (E, O)") {
    val CorrectPosition = new Position(2, 2, N())
    val validation1 = MapLimitsService.validateEO(CorrectPosition, mapLimitToTest)
    assert(validation1.isSuccess)

    val IncorrectPosition = new Position(7, 2, N())
    val validation2 = MapLimitsService.validateEO(IncorrectPosition, mapLimitToTest)
    assert(validation2.isFailure)
  }

  test("Map exception can be thrown, with a position") {
    val IncorrectPosition = new Position(7, 2, N())
    val ex = MapLimitsService.mapException(IncorrectPosition)
    assert(ex.isFailure)
  }

}
