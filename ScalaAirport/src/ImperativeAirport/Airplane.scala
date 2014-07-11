package ImperativeAirport

/**
 * Classe che Rappresenta un Aereo
 * Ogni Aereo ha:
 * 		-nome
 *   	-aeroporto di partenza
 *    	-aeroporto di arrivo
 * Un'aereo decolla (takeOff) e atterra (landing).
 * Quando un aereo deve decollare, lo comunica all'aeroporto di partenza.
 * 
 * @constructor	crea un aereo
 * @param	departure	l'aereoporto di partenza dell'aereo
 * @param	arrival		l'aeroporto di arrivo dell'aereo
 * @param	name		nome identificativo dell'aereo
 */

class Airplane (d: Airport, a: Airport, n:String){
	var name: String = n;
	var departure: Airport = d;
	var arrival: Airport = a;
	private var departed:Boolean = false;
	private var arrived:Boolean = false;
	
	def this() = this(null, null, "")
	
	def start():Unit = {
	  departure.addDeparture(this);
	  println(name + " aspetta di partire...");
	}
	
	def takeOff(late:Boolean): Unit  = {
	  departed = true;
	  println("Aereo " + name + " e decollato da " + d.name + " in ritardo? " + late);
	}
	
	def landing(late:Boolean): Unit = {
	  arrived = true;
	   println("Aereo " + name + " e atterrato a " + a.name + " in ritardo? " + late);
	}
	
}