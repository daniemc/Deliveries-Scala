package co.com.s4n.deliveries.domain.services

import co.com.s4n.deliveries.domain.VO.{A, E, L, Move, N, O, R, S}
import co.com.s4n.deliveries.domain.entities.{Drone, MapLimits, Position}

import scala.util.Try

sealed trait PositionAlgebra {
  def defaultPosition: Position
  def reposition(move: Move, drone: Drone, cityMap: MapLimits): Try[Drone]
  def advance(position: Position, cityMap: MapLimits): Try[Position]
}

sealed trait PositionInterpretation extends PositionAlgebra {
  override def defaultPosition: Position = new Position(0, 0, N())

  override def reposition(move: Move, drone: Drone, cityMap: MapLimits): Try[Drone] = move match {
    case A() => advance(drone.position, cityMap).map(newPos => new Drone(drone.name, drone.output, newPos))
    case L() => Try(new Drone(drone.name, drone.output, OrientationService.lFrom(drone.position)))
    case R() => Try(new Drone(drone.name, drone.output, OrientationService.rFrom(drone.position)))
  }

  override def advance(position: Position, cityMap: MapLimits): Try[Position] = position.o match {
    case N() => MapLimitsService.validate(new Position(position.x, position.y + 1, position.o), cityMap)
    case S() => MapLimitsService.validate(new Position(position.x, position.y - 1, position.o), cityMap)
    case O() => MapLimitsService.validate(new Position(position.x - 1, position.y, position.o), cityMap)
    case E() => MapLimitsService.validate(new Position(position.x + 1, position.y, position.o), cityMap)
  }
}

object PositionService extends PositionInterpretation
