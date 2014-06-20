package ImperativeAirport

import scala.collection.mutable.ListBuffer;
import scala.util.Random;


/**
 * Genera il sistema di aeroporti.
 * Occorre dare in input il numero di aeroporti e aerei da creare.
 * Durante la creazione degli aerei crea la timetable degli aeroporti.
 * A fine creazione, delega a due thread l'attivazione di aerei e aeroporti.
 */
object SystemGenerator {

  def isAllDigits(x: String) = x forall Character.isDigit
  
  def main(args: Array[String]): Unit = { 
    
    if(args.size < 2)
      println("Inserire due parametri numerici");
    else
      if(!isAllDigits(args(0)) || !isAllDigits(args(1)))
        println("Inserire due parametri numerici");
      else{
    val nAirport = args(0).toInt;
    val nPlanes = args(1).toInt;
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
    while(i <= nPlanes){
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
}