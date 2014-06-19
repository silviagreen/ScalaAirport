package ImperativeAirport



class Airport (n:String){
	val name = n;
	var track = new Object();	//pista d'atterraggio
	
	private var controlTower = new ControlTower(this);
	controlTower.start();
	println("Torre Controllo aeroporto " + name + " attivata" );
	
	def addDeparture(p:Airplane){
	  controlTower.addDeparture(p);
	}
	
	def addArrival(p:Airplane){
	  controlTower.addArrival(p);
	}
	
	
	
	
	
}