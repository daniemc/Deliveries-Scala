package co.com.s4n.deliveries.domain.entities

trait MapBuilder {
  val N: Int
  val E: Int
  val S: Int
  val O: Int
}
case class MapLimits(N: Int, E: Int, S: Int, O: Int) extends MapBuilder
