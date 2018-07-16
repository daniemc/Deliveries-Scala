import co.com.s4n.deliveries.domain.services.DroneService
import co.com.s4n.deliveries.infrastructure.FileAccess
import org.scalatest._

class test extends FunSuite {
  test("multi-Delivery") {
    val deliveriesList = FileAccess.list
    DroneService.multiDroneDelivery(deliveriesList)

  }
}
