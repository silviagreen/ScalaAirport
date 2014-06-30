package FunPlusSembraOOk

import scala.collection.mutable.Queue
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import akka.actor._
import akka.actor.Actor
import akka.actor.OneForOneStrategy
import akka.actor.Stash
import akka.actor.SupervisorStrategy._
import akka.pattern.ask
import akka.pattern.pipe
import scala.concurrent.Future
import scala.concurrent.Await
import akka.util.Timeout

class Aereo(p: Aeroporto, a: Aeroporto, n: String) {

  private val _name = n
  private val _partenza = p
  private val _arrivo = a

  def name = _name
  def partenza = _partenza
  def arrivo = _arrivo
}

//Messaggi
abstract class Messaggi
case class DecollaInRitardo(a: Aereo) extends Messaggi
case class AtterraInRitardo(a: Aereo) extends Messaggi
case class ChiediDecollo(a: Aereo) extends Messaggi
case class ChiediAtterraggio(a: Aereo) extends Messaggi
case object FaiDecollare extends Messaggi
case object FaiAtterrare extends Messaggi
case class Decolla(a: Aereo, ritardo: Boolean) extends Messaggi
case class Atterra(a: Aereo, ritardo: Boolean) extends Messaggi
case class Partiti(n:Int)
case class Arrivati(n:Int)
case class CodeTerminate

class Pista(ae: Aeroporto) extends Actor {
  private val aeroporto = ae
  private val partenze = ae.timetable count(_.equalsIgnoreCase("D"))
  private val arrivi = ae.timetable count(_.equalsIgnoreCase("A"))
  private var partiti = 0
  private var arrivati = 0
  def receive = {
    //case UsaPista(message : String) => println(message)
    case Decolla(a: Aereo, ritardo: Boolean) =>
      println(a.name + " decolla da " + aeroporto.nome + " (in ritardo? " + ritardo + ")"/* + " " + sender*/)
      partiti = partiti + 1
      if (partiti == partenze && arrivi == arrivati){ println("FINE"+ aeroporto.nome)
    	  aeroporto.manager ! CodeTerminate}
      Thread.sleep(1000)
      a.arrivo.richiestaAtterraggio ! ChiediAtterraggio(a)
     
    case Atterra(a: Aereo, ritardo: Boolean) => println(a.name + " atterra a " + aeroporto.nome + " (in ritardo? " + ritardo + ")"/* + " " + sender*/)
    											Thread.sleep(1000)
    											arrivati = arrivati + 1
    											if (partiti == partenze && arrivi == arrivati){  println("FINE " + aeroporto.nome)
    												aeroporto.manager ! CodeTerminate}
  }
  
   override val supervisorStrategy = OneForOneStrategy(){
    case _: NullPointerException => Resume
  }
   
  // override def postStop { println("TestActor::postStop pista") }

}

/*class MyList(l:List[Aereo]) {
	  var lista = l
	  
	  def ::(aereo : Aereo) = lista = aereo :: l
	  def dequeue = l match {
	    case Nil => throw new NoSuchElementException
	    case a :: rest => lista = rest
	    					a
	  }
}*/

class GestoreRitardi(a: Aeroporto) extends Actor {
  private val aeroporto = a
  def receive = {
    case DecollaInRitardo(a: Aereo) => println(a.name + " decolla in ritardo" )
      aeroporto.pista ! Decolla(a, true) //UsaPista(a.name + " decolla in ritardo da " + aeroporto.nome)

    case AtterraInRitardo(a: Aereo) => println(a.name + " atterra in ritardo" )
      aeroporto.pista ! Atterra(a, true) //UsaPista(a.name + " atterra in ritardo a " + aeroporto.nome)
  }
  
   override val supervisorStrategy = OneForOneStrategy(){
    case _: NullPointerException => Resume
  }
   
  // override def postStop { println("TestActor::postStop ritardi") }
}

class GestoreAtterraggi(a: Aeroporto) extends Actor {

  private val aeroporto = a
  private var arrivi = Queue[Aereo]()
  private var ritardiA = 0
 // private var fineDecolli = false
  //private var decolli = 0

  def receive = {

    case ChiediAtterraggio(a: Aereo) =>
      val mittente = sender
      ritardiA match {
        case 0 =>
          println(a.name + " in coda atterraggi")
          arrivi += a
        case x if x > 0 =>   println(a.name + " riparte subito, mancano " + ritardiA + " ritardi")
          ritardiA = ritardiA - 1
          //(aeroporto.pista) ! UsaPista(a.name + " atterra a " + a.arrivo.nome)
          aeroporto.GestoreRitardi ! AtterraInRitardo(a)
         // decolli = decolli + 1
      }

    case FaiAtterrare =>
      val mittente = sender
      Try(arrivi.dequeue) match {
        case Success(aereo) => println("ok atterraggio di  " + aereo.name)
     mittente ! Some(aereo)
        case Failure(f) =>
          ritardiA = ritardiA + 1
          println("nessun aereo da far decollare, ritardi " + ritardiA)
          mittente ! None

      }
      
   /* case FineDecolli => fineDecolli = true
    case FineRichiesteDecolli(x) => x - decolli match {
      case 0 => self ! PoisonPill
      case x if x > 0 => self ! FineRichiesteDecolli(x)
    }*/

  }
  
   override val supervisorStrategy = OneForOneStrategy(){
    case _: NullPointerException => Resume
  }
   
  // override def postStop { println("TestActor::postStop atterraggi") }
}

class GestoreDecolli(a: Aeroporto) extends Actor {
  private val aeroporto = a
  private var partenze = Queue[Aereo]()
  private var ritardi = 0


  def receive = {
    case ChiediDecollo(a: Aereo) => println(a.name + " chiede decollo")
      val mittente = sender
      ritardi match {
        case x if x > 0 =>  println(a.name + " riparte subito, mancano " + ritardi + " ritardi")
          ritardi = ritardi - 1
          //aeroporto.pista ! UsaPista(a.name + " decolla da " + a.partenza.nome)
          aeroporto.GestoreRitardi ! DecollaInRitardo(a)
        //a.arrivo.richiestaAtterraggio ! ChiediAtterraggio(a)

        case 0 => partenze += a
      }
    case FaiDecollare =>
      val mittente = sender

      Try(partenze.dequeue) match {
        case Success(aereo) =>   println("ok decollo di  " + aereo.name)
          mittente ! Some(aereo)
        case Failure(f) =>
          println("nessun aereo da far decollare, ritardi " + ritardi)
          ritardi = ritardi + 1
          mittente ! None
      }
  }
  
  override val supervisorStrategy = OneForOneStrategy(){
    case _: NullPointerException => Resume
  }
  
 // override def postStop { println("TestActor::postStop decolli") }
}

class Aeroporto(n: String) extends {

  implicit val system = ActorSystem("planes")
  var timetable = List[String]()
  
  private val _nome = n
  def nome = _nome

  private var _pista : ActorRef = _
  private val _richiestaDecollo = system.actorOf(Props(new GestoreDecolli(this)), name = "richiestaDecollo")
  private val _richiestaAtterraggio = system.actorOf(Props(new GestoreAtterraggi(this)), name = "richiestaAtterraggio")
  private val _gestoreRitardi = system.actorOf(Props(new GestoreRitardi(this)), name = "gestoreRitardi")
  private val _manager : ActorRef = system.actorOf(Props(new Manager(system)), name = "manager")

  def pista = _pista
  def richiestaDecollo = _richiestaDecollo
  def richiestaAtterraggio = _richiestaAtterraggio
  def GestoreRitardi = _gestoreRitardi
  def manager = _manager

  def timetable_(l: List[String]) = timetable = l


  def proxTransito: Future[Unit] = Future {
    _pista = system.actorOf(Props(new Pista(this)), name = "pista")
    implicit val timeout = Timeout(100 seconds)
   
    for (t <- timetable) yield {
      t match {
        case "D" =>

          val future = richiestaDecollo ? FaiDecollare
          val result = Await.result(future, timeout.duration).asInstanceOf[Option[Aereo]]
          result match {
            case Some(a) =>
              pista ! Decolla(a, false) //UsaPista(a.name + " decolla da " + a.partenza.nome)
            case None =>
          }


        case "A" =>
          val future = richiestaAtterraggio ? FaiAtterrare
          val results = Await.result(future, timeout.duration).asInstanceOf[Option[Aereo]]
          results match {
            case Some(a) => pista ! Atterra(a, false) //UsaPista(a.name + " atterra da " + a.arrivo.nome)
            case None =>
          }

      }
      

    }
    
    
  }


  
  def start = {proxTransito
    //proxTransito.onSuccess{ case t => system.shutdown}
  }

}

class Manager(system:ActorSystem) extends Actor {
  var finiti = 0

  def receive = {
    case CodeTerminate => system.shutdown

    
  }
}
  /*    
    case Partiti(n:Int) => println( partenze - n)
      partenze - n match {
      case 0 => a.richiestaDecollo ! PoisonPill
    	/*	  codaPartenzeChiusa = true
    		  codaArriviChiusa match {
        case true => self ! CodeTerminate
        case false =>
      }*/
      case x if x != 0 => 
    }
    
    case Arrivati(n:Int) => println( arrivi - n)
      arrivi - n match {
      case 0 => a.richiestaAtterraggio ! PoisonPill
    	/*	  	codaArriviChiusa = true
    		  	codaPartenzeChiusa match {
        case true => self ! CodeTerminate
        case false =>
      }*/
      case x if x != 0 => 
    }
    
    case CodeTerminate => system.shutdown//a.GestoreRitardi ! PoisonPill
    						//a.pista ! PoisonPill
    						
    						
    */
      
/*    case CodeTerminate => 
      println(a.nome + " SHUTDOWN")
      system.shutdown
      println(System.nanoTime())
  
  
}*/
 
