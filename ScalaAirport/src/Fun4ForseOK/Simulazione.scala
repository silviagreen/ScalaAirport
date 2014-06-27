package Fun4ForseOK

import scala.util.Random
import akka.actor.ActorSystem
import akka.actor.Props



object Simulazione {
  
  def isAllDigits(x: String) = x forall Character.isDigit
    
  def checkParameters(args: Array[String], nAeroporti: => String, nAerei: => String, correctType: String=>Boolean): Boolean = {
    if(args.size < 2){
      println("Errore: Sono richiesti due parametri numerici")
      false
    }
    else{
      if(!correctType(nAeroporti) || !correctType(nAerei)){
        println("Errore: inseriti parametri non numerici")
        false
      }else true
    }
  }
  
  var i = 0
  var j = 0

  def main(args: Array[String]): Unit = {
   	implicit val system = ActorSystem("planes")
    
    if(checkParameters(args, args(0), args(1), isAllDigits)){
    	val nAeroporti = args(0).toInt
    	val nAerei = args(1).toInt
    	val aeroporti =  1 to nAeroporti map{ _=> i = i + 1 
    	  										//	system.actorOf(Props(new Aeroporto("Aeroporto" + (i))), name = "aereoporto" + i)
      											 new Aeroporto("Aeroporto" + (i))
    	  
    	}
    
   /* for(i <- aeroporti)
      println(i.nome)*/
    
    val index = List.range(0, nAeroporti, 1)
    println("NOME\t PARTENZA\t ARRIVO")
    val aerei = 1 to nAerei map{
      _ => val idx = Random.shuffle(index)
    	   j = j + 1
    	   aeroporti(idx(0)).timetable += "D"
    	   aeroporti(idx(1)).timetable += "A"
    	   println("aereo" + j + "\t" + aeroporti(idx(0)).nome + "\t" + aeroporti(idx(1)).nome)
    	   new Aereo(aeroporti(idx(0)), aeroporti(idx(1)), "aereo" + j)  
    }
    
    
    println("\n")
    println("\n")
    aeroporti foreach (a => println(a.nome + "\t" + a.timetable))
     println("\n")
    println("\n")
    
    aeroporti foreach (a => a.start)
    aerei foreach (a => a.partenza.richiestaDecollo ! ChiediDecollo(a)/*a.partenza.richiestaAtterraggio(a)*/)
  
  }else{
    println("La simulazione non può partire")
  }
  }
}