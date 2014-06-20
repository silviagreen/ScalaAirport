package ImperativeAirport

import scala.collection.mutable.ListBuffer;
import scala.util.Random;

object SystemGenerator {

  def main(args: Array[String]): Unit = {
    val nAirport = 5;//Integer.getInteger(args(0));
    var airportList = ListBuffer[Airport]();
    val planeList = ListBuffer[Airplane]();
    
    println("N_AEROPORTI: " + nAirport);
    for(i <- 1 to nAirport){
      var airport = new Airport("airport" + i);
      airportList.append(airport); //+= airport;
    }
    
    var i = 1;
    
    println("---------------TIME TABLE----------------")
    println("NOME\t PARTENZA\t ARRIVO\t");
    while(i <= 20){
      airportList = Random.shuffle(airportList.toList).to[ListBuffer]; 
      var plane = new Airplane(airportList(0), airportList(1), "aereo" + i);

      airportList(0).timetable += "D";
      airportList(1).timetable += "A";
      
      planeList.append(plane);
      println(plane.name  + "\t" + plane.departure.name + "\t" + plane.arrival.name + "\t");
      i = i+1;   
    }
    
    val startAirports = new Thread(new Runnable {
    	def run() {
    		for(p <- airportList){
    		  p.activate();
    		}
    		
    	}
    });
    
    val startPlanes = new Thread(new Runnable() {
		def run() {
			for (p <- planeList){
			  p.start();
			  Thread.sleep(200);
			}
		}
	});
    
    startPlanes.start();
    startAirports.start();
    
    
  }

}