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
 * 
 * @constructor	crea una torre di controllo
 * @param	airport		l'aeroporto a cui la torre è associata
 */
class ControlTower (a:Airport) extends Thread{
	var airport:Airport = a;
	var nextTransit:String = "A";//indica il prossimo elemento della timetable da leggere
	private var arrivalsQueue: Queue[Airplane] = new Queue[Airplane];
	private var departuresQueue: Queue[Airplane] = new Queue[Airplane];
	private var timetable: ArrayBuffer[String] = a.getTimeTable;
	var i = 4;
	
	/**
	 * Metodo che aggiunge un aereo alla coda degli aerei
	 * in partenza
	 */
	def addDeparture(p:Airplane):Unit = {
	  departuresQueue.synchronized{
	    departuresQueue += p;
	    departuresQueue.notifyAll();
	  }
	  
	}
	
	/**
	 * Metodo che aggiunge un aereo alla coda degli aerei
	 * in arrivo
	 */
	def addArrival(p:Airplane):Unit = {
	  arrivalsQueue.synchronized{
	    arrivalsQueue += p;
	    arrivalsQueue.notifyAll();
	  }
	}
	
	/**
	 * Metodo che setta nextTransit con l'index-esimo
	 * elemento della timetable
	 */
	def setNextTransit(index:Int):Unit = {
	  nextTransit = timetable(index);	  
	}
	
	/**
	 * Assegna a un aereo la pista quando è libera
	 * 
	 * @param	p			l'aereo a cui assegnare la pista
	 * @param	isLanding	true se l'aereo deve atterrare, false se deve decollare
	 */
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
	
	/**
	 * Gestore delle partenze:
	 * se la coda degli aerei in partenza è vuota,
	 * crea un nuovo thread waiting per attendere la partenza in ritardo,
	 * altrimenti prende un aereo dalla coda e gli assegna la pista non appena
	 * essa è libera
	 */
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
	
	/**
	 * Gestore degli arrivi:
	 * se la coda degli aerei in arrivo è vuota,
	 * crea un nuovo thread waiting per attendere l'arrivo in ritardo,
	 * altrimenti prende un aereo dalla coda e gli assegna la pista non appena
	 * essa è libera
	 */
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

	/**
	 * Scorre la tabella oraria:
	 * ogni volta che legge una "A" deve gestire un arrivo,
	 * quando legge una "D" deve gestire un decollo
	 */
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