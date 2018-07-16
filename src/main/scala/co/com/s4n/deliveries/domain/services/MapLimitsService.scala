package co.com.s4n.deliveries.domain.services

import co.com.s4n.deliveries.domain.entities.{MapLimits, Position}

import scala.util.{Failure, Success, Try}

object MapLimitsService {
  def defaultMap: MapLimits = {
    new MapLimits(10, 10, -10, -10)
  }

  def validateNS(position: Position, cityMap: MapLimits): Try[Position] = {
    if (position.y >= cityMap.S && position.y <= cityMap.N) Success(position) else mapException(position)
  }

  def validateEO(position: Position, cityMap: MapLimits): Try[Position] = {
    if (position.x >= cityMap.O && position.x <= cityMap.E) Success(position) else mapException(position)
  }

  def mapException(position: Position): Try[Position] = {
    Failure(new Exception(s"Can't go beyond city map limits, Pos:($position)"))
  }

}
