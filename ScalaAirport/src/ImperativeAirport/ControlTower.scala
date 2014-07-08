package ImperativeAirport

import scala.collection.mutable.Queue;
import scala.collection.mutable.ArrayBuffer

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
	var airport:Airport = a;
	var nextTransit:String = "A";
	private var arrivalsQueue: Queue[Airplane] = new Queue[Airplane];
	private var departuresQueue: Queue[Airplane] = new Queue[Airplane];
	private var timetable: ArrayBuffer[String] = a.getTimeTable;
	var i = 4;
	
	def addDeparture(p:Airplane):Unit = {
	  departuresQueue.synchronized{
	    departuresQueue += p;
	    departuresQueue.notifyAll();
	  }
	  
	}
	
	def addArrival(p:Airplane):Unit = {
	  arrivalsQueue.synchronized{
	    arrivalsQueue += p;
	    arrivalsQueue.notifyAll();
	  }
	}
	
	def setNextTransit(index:Int):Unit = {
	  nextTransit = timetable(index);	  
	}
	
	def putPlaneOnTrack(p:Airplane, isLanding:Boolean) :Unit = {
	  if(isLanding){
		  airport.track.synchronized{
			  p.landing(false);
		  }
	  }else{
		  airport.track.synchronized{
		    p.takeOff(false);
		  }
		  Thread.sleep(500);
		  p.arrival.addArrival(p);
	  }
	}
	
	def handleDeparture(): Unit = {
	   var plane = new Airplane();
	  departuresQueue.synchronized{
	    if(departuresQueue.isEmpty){
	      println(airport.name + " ha coda partenze vuota, creo waiting");
	      new Waiting(departuresQueue, false).start();
	      return;
	    }else{
	      plane = departuresQueue.dequeue();
	    }
	  }	  	  
	    putPlaneOnTrack(plane, false);	  	
	}
	
	
	def handleArrival(): Unit = {
	  var plane = new Airplane();
	  arrivalsQueue.synchronized{
	    if(arrivalsQueue.isEmpty){
	      println(airport.name + " ha coda arrivi vuota, creo");
	      new Waiting(arrivalsQueue, true).start();
	      return;
	    }else{
	      plane = arrivalsQueue.dequeue();
	    }
	  } 
	    putPlaneOnTrack(plane, true);	  
	}

	override def run(){
	  timetable = a.getTimeTable;//devo aggiornarla perchè ci ho applicato randomize e startwithDeparture
	  println(airport.name + "\t" + timetable)
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