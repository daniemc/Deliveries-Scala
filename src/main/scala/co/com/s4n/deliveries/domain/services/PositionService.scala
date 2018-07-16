package co.com.s4n.deliveries.domain.services

import co.com.s4n.deliveries.domain.VO.{A, D, L, R, N, O, S, E, Move}
import co.com.s4n.deliveries.domain.entities.{MapLimits, Position}

import scala.util.Try

sealed trait PositionAlgebra {
  def defaultPosition: Position
  def reposition(move: Move, position: Try[Position], cityMap: MapLimits, name: String): Try[Position]
  def advance(position: Position, cityMap: MapLimits): Try[Position]
}

sealed trait PositionInterpretation extends PositionAlgebra {
  override def defaultPosition: Position = new Position(0, 0, N())

  override def reposition(move: Move, position: Try[Position], cityMap: MapLimits, name: String): Try[Position] = move match {
    case A() => advance(position.get, cityMap)
    case L() => Try(OrientationService.lFrom(position.get))
    case R() => Try(OrientationService.rFrom(position.get))
    case D() => Try(DroneService.deliverOrder(position.get, name))
  }

  override def advance(position: Position, cityMap: MapLimits): Try[Position] = position.o match {
    case N() => MapLimitsService.validateNS(new Position(position.x, position.y + 1, position.o), cityMap)
    case S() => MapLimitsService.validateNS(new Position(position.x, position.y - 1, position.o), cityMap)
    case O() => MapLimitsService.validateEO(new Position(position.x - 1, position.y, position.o), cityMap)
    case E() => MapLimitsService.validateEO(new Position(position.x + 1, position.y, position.o), cityMap)
  }
}

object PositionService extends PositionInterpretation
