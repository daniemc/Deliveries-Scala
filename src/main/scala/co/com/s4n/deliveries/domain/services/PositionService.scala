package co.com.s4n.deliveries.domain.services

import co.com.s4n.deliveries.domain.VO.{A, D, L, R, N, O, S, E, Moves}
import co.com.s4n.deliveries.domain.entities.{MapLimits, Position}

import scala.util.Try

object PositionService {
  def reposition(move: Moves, position: Try[Position], cityMap: MapLimits): Try[Position] = {
    move match {
      case A() => advance(position.get, cityMap)
      case L() => Try(OrientationService.lFrom(position.get))
      case R() => Try(OrientationService.rFrom(position.get))
      case D() => Try(DroneService.delivery(position.get))
    }
  }

  def advance(position: Position, cityMap: MapLimits): Try[Position] = {
    position.o match {
      case N() => MapLimitsService.validateNS(new Position(position.x, position.y + 1, position.o), cityMap)
      case S() => MapLimitsService.validateNS(new Position(position.x, position.y - 1, position.o), cityMap)
      case O() => MapLimitsService.validateEO(new Position(position.x - 1, position.y, position.o), cityMap)
      case E() => MapLimitsService.validateEO(new Position(position.x + 1, position.y, position.o), cityMap)
    }
  }

}
