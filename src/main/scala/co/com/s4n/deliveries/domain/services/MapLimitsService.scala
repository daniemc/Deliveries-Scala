package co.com.s4n.deliveries.domain.services

import co.com.s4n.deliveries.domain.entities.{MapLimits, Position}

import scala.util.{Failure, Success, Try}

sealed trait MapLimitsAlgebra {
  def defaultMap: MapLimits
  def validateNS(position: Position, cityMap: MapLimits): Try[Position]
  def validateEO(position: Position, cityMap: MapLimits): Try[Position]
  def mapException(position: Position): Try[Position]
}

sealed trait  MapLimitsInterpretation extends MapLimitsAlgebra {
  override def defaultMap: MapLimits = {
    new MapLimits(10, 10, -10, -10)
  }

  override def validateNS(position: Position, cityMap: MapLimits): Try[Position] = {
    if (position.y >= cityMap.S && position.y <= cityMap.N) Success(position) else mapException(position)
  }

  override def validateEO(position: Position, cityMap: MapLimits): Try[Position] = {
    if (position.x >= cityMap.O && position.x <= cityMap.E) Success(position) else mapException(position)
  }

  override def mapException(position: Position): Try[Position] = {
    Failure(new Exception(s"Can't go beyond city map limits, Pos:($position)"))
  }
}

object MapLimitsService extends MapLimitsInterpretation
