package ImperativeAirport

import scala.collection.mutable.Queue;

class Waiting (wq:Queue[Airplane], landing:Boolean) extends Thread{
	var queue = wq;
	var isLanding = landing;
	
	override def run(){
	  var plane = new Airplane(null,null,"");
	  queue.synchronized{
	    while(queue.isEmpty)
	      queue.wait();
	    
	    plane = queue.dequeue();
	  }
	  //è arrivato un aereo, può usare la pista
	  
	  if(landing){
		  plane.arrival.track.synchronized{
			  plane.landing();
		  }
		  
	  }else{
	    plane.arrival.track.synchronized{
	      plane.takeOff();
	    }
	    plane.arrival.addArrival(plane);
	  }
	}
}