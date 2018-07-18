package co.com.s4n.deliveries.infrastructure

import java.io.{BufferedWriter, File, FileWriter}
import scala.util.{Failure, Try}

object FileAccess {
  val rootPath = System.getProperty("user.dir")
  val basePath = "src/main/resources"
  def fullPath = {
    s"${ rootPath }/${ basePath }"
  }


  def read(fileName: String): Try[List[String]] = {
    Try(scala.io.Source.fromFile(s"$fullPath/$fileName").getLines().toList)
  }

  def write(fileName: String, message: String) = {
      Try {
        val path = s"${fullPath}/$fileName"
        val writer = new BufferedWriter(new FileWriter(path, true))
        writer.write(message)
        writer.newLine()
        writer.close()
      }
  }

  def list(path: String): Try[List[String]] = {
    Try( new File(path).listFiles
      .filter(file => file.isFile)
      .map(file => file.getName)
      .filter(file => file.startsWith("in") && file.endsWith("txt"))
      .filter(file => file.length == 8)
      .toList )
  }

  // TODO:
  // file.read
  //  .map(listDeliveries: List[String] => listDeliveries
  //    .map(lineDeliver => getAddress(lineDeliver))
  //      .map(addressList => Delivery(addressList))
  //
  // Quitar throws
}
