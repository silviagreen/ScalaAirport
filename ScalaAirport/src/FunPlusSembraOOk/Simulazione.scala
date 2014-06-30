package FunPlusSembraOOk

import scala.util.Random
import akka.actor.ActorSystem
import com.sun.xml.internal.ws.wsdl.writer.document.StartWithExtensionsType



object Simulazione {
  
  /**
   * def fraction: PartialFunction[Int, Int] =
    { case d: Int if d != 0 ⇒ 42 / d }
   */
  
/*class IntToStringConverter extends Int => String {
  def apply(i: Int): String = i.toString
}*/
  

  
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
   //	implicit val system = ActorSystem("planes")
   	implicit def strToInt(x: String) = x.toInt
    
    checkParameters(args, args(0), args(1)/*, isAllDigits*/) match {
   	  case false => println("La simulazione non può partire")
   	  case true => val nAeroporti:Int = args(0)//.toInt
    	val nAerei:Int = args(1)//.toInt
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
    	
    	aeroporti map{	a:Aeroporto => val tt = new PreparaTimetable(a)   with StartWithDeparture with Normalizza   with Randomize
    									a.timetable = tt.trasforma(tt.recInit((aerei filter(_.partenza.equals(a)) ).toList  ++  (aerei filter(_.arrivo.equals(a))).toList))
    									//a.timetable = tt.stringList
    									
    	}
 
    
    //----------------------------------------------------------------------------
    
    println("\n")
    println("\n")
    aeroporti foreach (a => println(a.nome + "\t" + a.timetable))
     println("\n")
    println("\n")
   
    println(System.nanoTime())
    aerei foreach (a => a.partenza.richiestaDecollo ! ChiediDecollo(a)/*a.partenza.richiestaAtterraggio(a)*/)
    
    
    aeroporti foreach {a => a.start
      					Thread.sleep(1500)			}
    
    
    
    //11.011812466 Seconds
  
  }
   
  
  }
}