package ImperativeAirport.FunctionalAirport

import scala.collection.mutable.ListBuffer;
import scala.concurrent.Future
import akka.actor.ActorSystem
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

class Plane(d:Airport, a : Airport, n:String){
  val name = n
  val start = d
  val end = a
}
class Airport(n:String){
  val name = n
}

object SystemGenerator {
  
  val airports = new ListBuffer[Airport]()
  
 

  def main(args: Array[String]): Unit = {
     implicit val system = ActorSystem("future")
     def generateAirports:Future[Unit] = Future { 
       
       for(i <- 0 to 2/*args(0).toInt*/){
    	airports += new Airport("aeroporto" + i)
    	//airports(i - 1).start();
	  println(airports(i).name)
     }
  	}
     
     def generatePlanes:Future[Unit] = Future {
       println("NOME\t PARTENZA\t ARRIVO")
       var index = List.range(0, args(0).toInt, 1)
       for(i <- 1 to args(1).toInt){
         index = Random.shuffle(index) 
         val p = new Plane(airports(index(0)), airports(index(1)), "aereo" + i)//.start();
         println(p.name + "\t " + p.start.name + "\t " + p.end.name)
       }
     }
     
     
     
     def genera():Future[Unit] = {
      
     // for( i <- generateAirports ) yield generatePlanes
    generateAirports
    
    generatePlanes
    
     }
     genera()
       
    
    
  }

}