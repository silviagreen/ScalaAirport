package ImperativeAirport

import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ResizableArray

/**
 * Genera il sistema di aeroporti.
 * Occorre dare in input il numero di aeroporti e aerei da creare.
 * Durante la creazione degli aerei crea la timetable degli aeroporti.
 * A fine creazione, si ha l'attivazione di aerei e aeroporti.
 *
 * Ogni tabella oraria è costruita casualmente ma sempre con una D come primo elemento
 */
object SystemGenerator {

  def isAllDigits(x: String): Boolean = {
    var chars = x.toCharArray()
    for (x <- chars) {
      if (!x.isDigit)
        return false
    }
    return true
  }

  def onlyArrivalsOrEmpty(a: ArrayBuffer[String]): Boolean = {
    if (a.size == 0) return true
    for (s <- a) {
      if (s.equalsIgnoreCase("D"))
        return false
    }
    return true
  }

  def swap[T](array: ArrayBuffer[T], a: Int, b: Int): ArrayBuffer[T] = {
    val h = array(a)
    array(a) = array(b)
    array(b) = h
    return array
  }

  def startWithDeparture(l: ArrayBuffer[String]): ArrayBuffer[String] = {
    if (l(0).equalsIgnoreCase("D")) return l
    else {
      var index = l.indexOf("D");
      var timetable = swap[String](l, 0, index)
      return timetable
    }

  }

  def main(args: Array[String]): Unit = {

    if (args.size < 2)
      println("Inserire due parametri numerici");
    else if (!isAllDigits(args(0)) || !isAllDigits(args(1)))
      println("Inserire due parametri numerici");
    else {
      var nAirport = args(0).toInt;
      var nPlanes = args(1).toInt;
      var airportList = ListBuffer[Airport]();
      var planeList = ListBuffer[Airplane]();

      println("N_AEROPORTI: " + nAirport);
      for (i <- 1 to nAirport) {
        var airport = new Airport("airport" + i);
        airportList.append(airport); //+= airport;
      }

      var i = 1;

      println("---------------TIME TABLE----------------")
      println("NOME\t PARTENZA\t ARRIVO\t");
      while (i <= nPlanes) {
        airportList = Random.shuffle(airportList.toList).to[ListBuffer];
        var plane = new Airplane(airportList(0), airportList(1), "aereo" + i);

        airportList(0).timetable += "D";
        airportList(1).timetable += "A";

        planeList.append(plane);
        println(plane.name + "\t" + plane.departure.name + "\t" + plane.arrival.name + "\t");
        i = i + 1;
      }

      val startAirports = new Thread(new Runnable {
        def run() {
          for (p <- airportList) {
            if (!onlyArrivalsOrEmpty(p.timetable)) {
              p.timetable = Random.shuffle(p.timetable)
              p.timetable = startWithDeparture(p.timetable)
            }
            println(p.name + "\t" + p.timetable)
            p.activate;
          }

        }
      });

      //partenze mai in ritardo  
      for (p <- planeList) {
        p.start();
        Thread.sleep(1500);
      }

      startAirports.start();

    }
  }
}