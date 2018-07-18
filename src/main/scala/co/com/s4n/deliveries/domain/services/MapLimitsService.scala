package co.com.s4n.deliveries.domain.services

import co.com.s4n.deliveries.domain.entities.{MapLimits, Position}

import scala.util.{Failure, Success, Try}

sealed trait MapLimitsAlgebra {
  def defaultMap: MapLimits
  def validate(position: Position, cityMap: MapLimits): Try[Position]
}

sealed trait  MapLimitsInterpretation extends MapLimitsAlgebra {
  override def defaultMap: MapLimits = {
    new MapLimits(10, 10, -10, -10)
  }

  override def validate(position: Position, cityMap: MapLimits): Try[Position] = Try {
    if(position.y >= cityMap.S && position.y <= cityMap.N && position.x >= cityMap.O && position.x <= cityMap.E)
      position
    else
      throw new Exception(s"Can't go beyond city map limits, Pos:($position)")
  }
}

object MapLimitsService extends MapLimitsInterpretation
