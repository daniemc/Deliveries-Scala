package co.com.s4n.deliveries.domain.services

import co.com.s4n.deliveries.domain.VO.{E, N, O, S}
import co.com.s4n.deliveries.domain.entities.{Position}

sealed trait OrientationAlebra {
  def lFrom(position: Position): Position
  def rFrom(position: Position): Position
}

sealed trait OrientationInterpretation extends OrientationAlebra {
  override def lFrom(position: Position): Position = position.o match {
    case N() => new Position(position.x, position.y, O())
    case E() => new Position(position.x, position.y, N())
    case S() => new Position(position.x, position.y, E())
    case O() => new Position(position.x, position.y, S())
  }

  override def rFrom(position: Position): Position = position.o match {
    case N() => new Position(position.x, position.y, E())
    case O() => new Position(position.x, position.y, N())
    case S() => new Position(position.x, position.y, O())
    case E() => new Position(position.x, position.y, S())
  }
}

object OrientationService extends OrientationInterpretation
