package ImperativeAirport

/**
 * Classe che Rappresenta un Aereo
 * Ogni Aereo ha:
 * 		-nome
 *   	-aeroporto di partenza
 *    	-aeroporto di arrivo
 * Un'aereo decolla (takeOff) e atterra (landing).
 * Quando un aereo deve decollare, lo comunica all'aeroporto di partenza
 */

class Airplane (d: Airport, a: Airport, n:String){
	var name = n;
	var departure = d;
	var arrival = a;
	private var departed = false;
	private var arrived = false;
	
	def start(){
	  departure.addDeparture(this);
	  println(name + " aspetta di partire...");
	}
	
	def takeOff(late:Boolean){
	  departed = true;
	  println("Aereo " + name + " e decollato da " + d.name + " in ritardo? " + late);
	}
	
	def landing(late:Boolean){
	  arrived = true;
	   println("Aereo " + name + " e atterrato a " + a.name + " in ritardo? " + late);
	}
	
}