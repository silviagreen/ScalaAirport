package ImperativeAirport

import scala.collection.mutable.ArrayBuffer

/**
 * Classe che rappresenta un Aeroporto
 * Ogni Aerporto ha:
 * 		-un nome
 *   	-una pista di decollo/atterraggio (track)
 *    	-la tabella dei decolli e atterraggi, in ordine di orario (timetable)
 *     	-la torre di controllo, che gestisce la pista
 * Quando un Aeroporto deve far atterrare/decollare un aereo, fa richiesta alla torre di controllo
 */
class Airport (n:String){
	var name: String= n;
	var track:Object = new Object();	//pista d'atterraggio
	var timetable:ArrayBuffer[String] = ArrayBuffer[String]();
	private var controlTower:ControlTower = new ControlTower(this);
	
	
	
	def addDeparture(p:Airplane):Unit  = {
	  controlTower.addDeparture(p);
	}
	
	def addArrival(p:Airplane):Unit = {
	  controlTower.addArrival(p);
	}
	
	def activate:Unit = {
	  controlTower.start();
	  println("Torre di Controllo aeroporto di " + name + " attivata" );
	}
	
	def getTimeTable :ArrayBuffer[String] = {
	  timetable;
	}
	
	
}