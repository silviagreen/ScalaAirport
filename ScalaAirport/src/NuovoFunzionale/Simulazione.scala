package NuovoFunzionale

import Reaper._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.ActorRef

object Simulazione extends App{


  /*class IntToStringConverter extends Int => String {
  def apply(i: Int): String = i.toString
}*/
  
  val checkIfDigit = (x:String) => x forall Character.isDigit

  def isStringDigit: PartialFunction[Any, Boolean] = {
    case x: String => checkIfDigit(x)
  }
  
  def isCharDigit: PartialFunction[Any, Boolean] = {
    case x: Char => x.isDigit
  }
  
  def isInteger: PartialFunction[Any, Boolean] = {
    case x: Int => true
  }
  
  def otherTypes: PartialFunction[Any, Boolean] = {
    case _ => false
  }
  
  def checkInput(x:Any) = (isInteger orElse isStringDigit orElse isCharDigit orElse otherTypes)(x)
  
 

  def checkParameters(args: Array[String], nAeroporti: => String, nAerei: => String /*, correctType: String=>Boolean*/ ): Boolean = {
    args.size match {
      case x if x < 2 => println("Errore: Sono richiesti due parametri numerici")
    		  			false
      case x if x >= 2 => (!(checkInput(nAeroporti) )|| !checkInput(nAerei)) match {
        case true => println("Errore: inseriti parametri non numerici")
        			 false
        case false => true
      }
    }

  
  }

  var i = 0
  var j = 0
  
  def nomeAeroporto = {
    i = i + 1
    "Aeroporto" + i}
  def nomeAereo = { 
    j = j + 1
    "Aereo" + j} 

  def fallisci = println("La simulazione non può partire")
  
  def decolloAerei(aerei : List[Aereo]) =  {
    aerei.par foreach {a => a.partenza ! ChiediDecollo(a) 
      					Thread.sleep(1500)}
  }
  
  def attivazioneAeroporti (aeroporti : List[ActorRef]) =  {
        aeroporti.par foreach ( a => a ! Start)
  }

  def creaSistema(n1: String, n2: String) = {

    implicit def strToInt(x: String) = x.toInt
    implicit def IndexSeqToList[T](x: IndexedSeq[T]) = x.toList
    implicit val system = ActorSystem("planes")
    
    val reaper = system.actorOf(Props(new ProductionReaper))
    
    val nAeroporti: Int = n1 //.toInt
    val nAerei: Int = n2 //.toInt
    val aeroporti = 1 to nAeroporti map { _ =>
   //   i = i + 1
      val nome = nomeAeroporto
    system.actorOf(Props(new Aeroporto(/*"Aeroporto" + i*/ nome )), name = /*"Aeroporto" + i*/nome)
      //new Aeroporto("Aeroporto" + (i))

    }

    println("NOME"+ "\t" + "PARTENZA" + "\t" + "ARRIVO")
    val index = List.range(0, nAeroporti, 1)
    val aerei = 1 to nAerei map {
      _ =>
        val idx = Random.shuffle(index)
      //  j = j + 1
        val nome = nomeAereo
        println(nome + "\t" + aeroporti(idx(0)).path + "\t" + aeroporti(idx(1)).path)
        new Aereo(aeroporti(idx(0)), aeroporti(idx(1)), /*"aereo" + j*/ nome)
    }

    aeroporti map { a: ActorRef =>
      val tt = new PreparaTimetable(a) with StartWithDeparture with Normalizza with Randomize
      //a.timetable = tt.trasforma(tt.recInit((aerei filter (_.partenza.equals(a))).toList ++ (aerei filter (_.arrivo.equals(a))).toList))
      a ! setTimetable(tt.trasforma(tt.recInit((aerei filter (_.partenza.equals(a))).toList ++ (aerei filter (_.arrivo.equals(a))).toList)))
      reaper ! WatchMe(a)
    }


    //println("\n")
    //aeroporti foreach (a => println(a.path + "\t" + a.timetable))
    //println("\n")

    //println(System.nanoTime())
    decolloAerei(aerei)
    attivazioneAeroporti(aeroporti)
    
    

  
    
    



    //11.011812466 Seconds

  }

//  def main(args: Array[String]): Unit = {

    checkParameters(args, args(0), args(1) /*, isAllDigits*/ ) match {
      case false => fallisci
      case true => creaSistema(args(0), args(1))

    }     // a.timetable.reverse
  //}
}

  /*aeroporti foreach ( a =>
    	  		a.timetable = Random.shuffle(List.tabulate((aerei filter(_.arrivo.equals(a))).size )(n => "A") ++ List.tabulate((aerei filter(_.partenza.equals(a))).size)(a => "D"))
    	    )*/