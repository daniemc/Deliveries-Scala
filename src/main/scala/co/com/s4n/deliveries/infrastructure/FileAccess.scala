package co.com.s4n.deliveries.infrastructure

import java.io.{BufferedWriter, File, FileWriter}

object FileAccess {
  val rootPath = System.getProperty("user.dir")
  val basePath = "src/main/resources"
  def fullPath = {
    s"${ rootPath }/${ basePath }"
  }

  @throws(classOf[Exception])
  def read(fileName: String): List[String] = {
    var lines : List[String] = List()
    val bufferedSource = scala.io.Source.fromFile(s"$fullPath/$fileName")
    bufferedSource.getLines().foreach(fileLine => {
      lines = lines :+ fileLine
    })
    bufferedSource.close()
    lines
  }

  @throws(classOf[Exception])
  def write(fileName: String, message: String) = {
    if (message != "") {
      val path = s"${fullPath}/$fileName"
      val writer = new BufferedWriter(new FileWriter(path, true))
      writer.write(message)
      writer.newLine()
      writer.close()
    } else {
      throw new Exception("Write failed: A message must be provided")
    }
  }

  @throws(classOf[Exception])
  def list: List[String] = {
    new File(fullPath).listFiles
      .filter(file => file.isFile)
      .map(file => file.getName)
      .filter(file => file.startsWith("in") && file.endsWith("txt"))
      .filter(file => file.length == 8)
      .toList
  }

  // TODO:
  // file.read
  //  .map(listDeliveries: List[String] => listDeliveries
  //    .map(lineDeliver => getAddress(lineDeliver))
  //      .map(addressList => Delivery(addressList))
  //
  // Quitar throws
}
