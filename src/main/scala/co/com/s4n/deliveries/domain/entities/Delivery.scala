package co.com.s4n.deliveries.domain.entities

import co.com.s4n.deliveries.domain.VO.Move

import scala.util.Try

case class Delivery(route : List[Try[Move]])
