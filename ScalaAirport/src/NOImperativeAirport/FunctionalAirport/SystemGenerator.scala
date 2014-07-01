package NOImperativeAirport.FunctionalAirport

import scala.collection.mutable.ListBuffer;
import scala.concurrent.Future
import akka.actor.ActorSystem
import akka.actor.{ Actor, Props, Terminated }
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

class Plane(d:Airport, a : Airport, n:String){
  val name = n
  val start = d
  val end = a
}
class Airport(n:String){
  implicit val system = ActorSystem("planes")
  val name = n
  var timetable = new ArrayBuffer[String]()
  val controlTower = system.actorOf(Props(new ControlTower(this)), name = "controltower")
  
  val pista = system.actorOf(Props(new Pista()), name = "pista")
  
}

class ControlTower(a:Airport) extends Actor{
	val airport = a
	var index = 0
	def receive = {
	  case ("D",p:Plane) => if(a.timetable(index).equals("D")){
		  						a.pista ! p.name + " decolla";
		  						self ! ("A", p)
		  						index = index + 1 
	  						}else{
	  						  self ! ("D", p)
	  						}
	    
	  case ("A", p:Plane) => if(a.timetable(index).equals("A")){
	    						a.pista ! p.name + " atterra";
	    						index = index + 1
	  							}else{
	  							  self ! ("A", p)
	  							}
	}
}

class Pista extends Actor {
  def receive = { 
     case x:String => println(x)
  }
}



object SystemGenerator {
  
  val airports = new ListBuffer[Airport]()
  val planes = new ListBuffer[Plane]()
 

  def main(args: Array[String]): Unit = {
     
     def generateAirports:Unit =  { 
       
       for(i <- 0 to 2/*args(0).toInt*/){
    	airports += new Airport("aeroporto" + i)
	  println(airports(i).name)
     }
  	}
     
     def generatePlanes:Unit =  {
       println("NOME\t PARTENZA\t ARRIVO")
       var index = List.range(0, args(0).toInt, 1)
       for(i <- 1 to args(1).toInt){
         index = Random.shuffle(index) 
         val p = new Plane(airports(index(0)), airports(index(1)), "aereo" + i)//.start();
         println(p.name + "\t " + p.start.name + "\t " + p.end.name)
         airports(index(0)).timetable += "D"
         airports(index(1)).timetable += "A" 
         planes += p
         
       }
     }
     
     def start = {
       for(p <- planes)
    	p.start.controlTower ! ("D",p)
     }
     
     def genera():Unit = {
      
     // for( i <- generateAirports ) yield generatePlanes
    generateAirports
    
    generatePlanes

     }
    // for(i <- genera()) yield start
     genera()
     start
      
       
    
    
    
  }

}