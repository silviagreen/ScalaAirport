package ImperativeAirport

import scala.collection.mutable.Queue;

/**
 * Classe che rappresenta una Torre di Controllo di un aeroporto.
 * Ogni Torre di Controllo gestisce:
 * 		-la pista dell'aeroporto
 *   	-la lista degli aerei che attendono di decollare (departuresQueue)
 *    	-la lista degli aerei che attendono di atterrare (arrivalsQueue)
 *     tutto secondo la tabella oraria (timetable) dell'aeroporto a cui la torre appartiente.
 * Quando un aereo è un ritardo, delega un thread per attenderlo e farlo atterrare/decollare non appena la pista si libera.
 */
class ControlTower (a:Airport) extends Thread{
	var airport = a;
	var nextTransit = "A";
	private var arrivalsQueue = new Queue[Airplane];
	private var departuresQueue = new Queue[Airplane];
	private val timetable = a.getTimeTable;
	var i = 4;
	
	def addDeparture(p:Airplane) = {
	  departuresQueue.synchronized{
	    departuresQueue += p;
	    departuresQueue.notifyAll();
	  }
	  
	}
	
	def addArrival(p:Airplane) = {
	  arrivalsQueue.synchronized{
	    arrivalsQueue += p;
	    arrivalsQueue.notifyAll();
	  }
	}
	
	def setNextTransit(index:Int) = {
	 /* if(nextTransit.equals("A"))
	    nextTransit = "D";
	  else
	    nextTransit = "A";*/
	  nextTransit = timetable(index);
	  
	}
	
	def putPlaneOnTrack(p:Airplane, isLanding:Boolean) = {
	  if(isLanding){
		  a.track.synchronized{
			  p.landing();
		  }
	  }else{
		  a.track.synchronized{
		    p.takeOff();
		  }
		  Thread.sleep(500);
		  p.arrival.addArrival(p);
	  }
	}
	
	def handleDeparture() {
	   var plane = new Airplane(null, null, "");
	  departuresQueue.synchronized{
	    if(departuresQueue.isEmpty){
	      println("coda partenze vuota, creo");
	      new Waiting(departuresQueue, false).start();
	      return;
	    }else{
	      plane = departuresQueue.dequeue();
	    }
	  }
	  
	  
	    putPlaneOnTrack(plane, false);
	  
	
	}
	
	
	def handleArrival(){
	  var plane = new Airplane(null, null, "");
	  arrivalsQueue.synchronized{
	    if(arrivalsQueue.isEmpty){
	      println("coda arrivi vuota, creo");
	      new Waiting(arrivalsQueue, true).start();
	      return;
	    }else{
	      plane = arrivalsQueue.dequeue()
	    }
	  }
	  
	 
	    putPlaneOnTrack(plane, true);
	  
	}
	
	/*def getDepartures(){
	  return departuresQueue;
	}
	
	def getArrivals(){
	  return arrivalsQueue;
	}*/
	
	override def run(){
	  var k = 1;
	  while(k <= timetable.size){
	    setNextTransit(k - 1);
	    if(nextTransit.equals("A")){
	      handleArrival();
	     
	     }
	    else{
	      handleDeparture();
	      
	       }
	    k = k + 1;
	    Thread.sleep(2000);
	  }
	  
	}
	
	
}