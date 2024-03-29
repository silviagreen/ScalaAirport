package NOFun4ForseOK

import scala.util.Random

import akka.actor.ActorSystem



object Simulazione {
  
  /**
   * def fraction: PartialFunction[Int, Int] =
    { case d: Int if d != 0 ⇒ 42 / d }
   */
  
 
  
  def isAllDigit : PartialFunction[String, Boolean] = {
    case x:String => x forall Character.isDigit 
  }
  
  //def isAllDigits(x: String) = x forall Character.isDigit
    
  def checkParameters(args: Array[String], nAeroporti: => String, nAerei: => String/*, correctType: String=>Boolean*/): Boolean = {
    if(args.size < 2){
      println("Errore: Sono richiesti due parametri numerici")
      false
    }
    else{
      if(!isAllDigit(nAeroporti) || !isAllDigit(nAerei)){
        println("Errore: inseriti parametri non numerici")
        false
      }else true
    }
  }
  
  var i = 0
  var j = 0

  def main(args: Array[String]): Unit = {
   	implicit val system = ActorSystem("planes")
    
    if(checkParameters(args, args(0), args(1)/*, isAllDigits*/)){
    	val nAeroporti = args(0).toInt
    	val nAerei = args(1).toInt
    	val aeroporti =  1 to nAeroporti map{ _=> i = i + 1 
    	  										//	system.actorOf(Props(new Aeroporto("Aeroporto" + (i))), name = "aereoporto" + i)
      											 new Aeroporto("Aeroporto" + (i))
    	  
    	}
    
   //----------------------MODO 1-------------------------------------------------------
    
   /* val index = List.range(0, nAeroporti, 1)
    println("NOME\t PARTENZA\t ARRIVO")
    val aerei = 1 to nAerei map{
      _ => val idx = Random.shuffle(index)
    	   j = j + 1
    	   aeroporti(idx(0)).timetable += "D"
    	   aeroporti(idx(1)).timetable += "A"
    	   println("aereo" + j + "\t" + aeroporti(idx(0)).nome + "\t" + aeroporti(idx(1)).nome)
    	   new Aereo(aeroporti(idx(0)), aeroporti(idx(1)), "aereo" + j)  
    }*/
    	val index = List.range(0, nAeroporti, 1)
    	val aerei =  1 to nAerei map{
    	  _=> val idx = Random.shuffle(index)
    	   j = j + 1
    	   println("aereo" + j + "\t" + aeroporti(idx(0)).nome + "\t" + aeroporti(idx(1)).nome)
    	   new Aereo(aeroporti(idx(0)), aeroporti(idx(1)), "aereo" + j)  
    	}
    	
    	/*aeroporti foreach ( a =>
    	  		a.timetable = Random.shuffle(List.tabulate((aerei filter(_.arrivo.equals(a))).size )(n => "A") ++ List.tabulate((aerei filter(_.partenza.equals(a))).size)(a => "D"))
    	    )*/
    	
    	aeroporti map{	a:Aeroporto => val tt = new PreparaTimetable(a) with Normalizza with Randomize
    									tt transforma((aerei filter(_.arrivo.equals(a)) ).toList  ++  (aerei filter(_.partenza.equals(a))).toList)
    									a.timetable = tt.stringlist
    	    
    	}
    
    //----------------------------------------------------------------------------
    
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