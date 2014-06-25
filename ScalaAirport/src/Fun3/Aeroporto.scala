package Fun3



import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import scala.concurrent.Future
import akka.actor.ActorSystem
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import akka.actor.ActorSystem
import akka.actor.{ Actor, Props, Terminated }
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import akka.actor.Stash


class Aereo(p: Aeroporto, a:Aeroporto, n:String){
  
  private val _name = n
  private val _partenza = p
  private val _arrivo = a
  
  def name = _name
  def partenza = _partenza
  def arrivo = _arrivo
}
//Messaggi
case class Decolla(a:Aereo)
case class Atterra(a:Aereo)
case class ChiediDecollo(a:Aereo)
case class ChiediAtterraggio(a:Aereo)
case object FaiDecollare
case object FaiAtterrare


class Pista extends Actor{
  def receive = {
    case Decolla(a:Aereo) =>//a.arrivo.richiestaAtterraggio(a) 
      						println(a.name + " decolla da " + a.partenza.nome)
      						a.arrivo.richiestaAtterraggio ! ChiediAtterraggio(a)
    						
    case Atterra(a:Aereo) => println(a.name + " atterra a " + a.arrivo.nome)
  }
}

class GestoreAtterraggi(a:Aeroporto) extends Actor with Stash{
  private val aeroporto = a
  
  def receive = {								   
    case FaiAtterrare => 
      unstashAll
      context.become({
         case ChiediAtterraggio(p:Aereo) => 
           aeroporto.pista ! Atterra(p)
           unstashAll()
           context.unbecome()
         case msg ⇒ stash()
        		 		
      }, discardOld = false)
      
    case msg ⇒ stash()
  }
}

class GestoreDecolli(a:Aeroporto) extends Actor with Stash{
  private val aeroporto = a
 
  def receive = { 										
    case FaiDecollare => 
      unstashAll
      context.become({
        case ChiediDecollo(p:Aereo) => 
          aeroporto.pista ! Decolla(p)
           unstashAll()
           context.unbecome()
        case msg ⇒ stash()
      }, discardOld = false)
      
     case msg ⇒ stash()
  }
}

class Aeroporto(n:String){
  
	implicit val system = ActorSystem("planes")
	val timetable = ListBuffer[String]()	
	private val _nome = n
	def nome = _nome

	private val _pista = system.actorOf(Props(new Pista), name = "pista")
	private val _richiestaDecollo = system.actorOf(Props(new GestoreDecolli(this)), name = "richiestaDecollo")
	private val _richiestaAtterraggio = system.actorOf(Props(new GestoreAtterraggi(this)), name = "richiestaAtterraggio")
	
	def pista = _pista
	def richiestaDecollo = _richiestaDecollo
	def richiestaAtterraggio = _richiestaAtterraggio
	
	def proxTransito : Future[Unit] = Future {
	  timetable foreach { t =>
	    t match {
	    case "D" => richiestaDecollo ! FaiDecollare
	    case "A" => richiestaAtterraggio ! FaiAtterrare
	  }
	  }
	}
	
	
	def start = proxTransito
	
}