package ImperativeAirport



class Airplane (d: Airport, a: Airport, n:String){
	val name = n;
	private val departure = d;
	val arrival = a;
	private var departed = false;
	private var arrived = false;
	
	def start(){
	  d.addDeparture(this);
	}
	
	def takeOff(){
	  departed = true;
	  println("Aereo " + name + " e decollato da " + d.name);
	}
	
	def landing(){
	  arrived = true;
	   println("Aereo " + name + " e atterrato a " + a.name);
	}
	
	/*def print(){
	  if(departed && ! arrived)
		  println("Aereo " + n + "e decollato");
	  else if(departed && arrived)
		  println("Aereo " + n + "e atterrato");
	}*/
}