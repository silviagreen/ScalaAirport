package ImperativeAirport

import scala.collection.mutable.ArrayBuffer

class Airport (n:String){
	val name = n;
	var track = new Object();	//pista d'atterraggio
	var timetable = ArrayBuffer[String]();
	private var controlTower = new ControlTower(this);
	
	
	
	def addDeparture(p:Airplane) = {
	  controlTower.addDeparture(p);
	}
	
	def addArrival(p:Airplane) = {
	  controlTower.addArrival(p);
	}
	
	def activate() = {
	  controlTower.start();
	  println("Torre di Controllo aeroporto " + name + " attivata" );
	}
	
	def getTimeTable():ArrayBuffer[String] = {
	  return timetable;
	}
	
	
}