package ImperativeAirport

import scala.collection.mutable.Queue;

/**
 * Classe che si occupa di aspettare un aereo in ritardo e di assegnargli la pista non appena si libera.
 * Si usa in modo da non bloccare la circolazione degli aerei a causa di un ritardo
 * 
 * Ritardo = coda delle partenze (o arrivi) vuota
 */
class Waiting (wq:Queue[Airplane], landing:Boolean) extends Thread{
	var queue : Queue[Airplane] = wq;
	var isLanding : Boolean = landing;
	
	override def run(){
	  var plane = new Airplane();
	  queue.synchronized{
	    while(queue.isEmpty)
	      queue.wait();
	    //se si sveglia significa che la coda non è più vuota
	    plane = queue.dequeue();
	  }
	  //è arrivato un aereo, può usare la pista
	  
	  if(isLanding){
		  plane.arrival.track.synchronized{
			  plane.landing(true);
		  }
		  
	  }else{
	    plane.departure.track.synchronized{
	      plane.takeOff(true);
	    }
	    Thread.sleep(500);
	    plane.arrival.addArrival(plane);
	  }
	  
	  
	}
}