package FunctionalAirport2


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

class GestoreAtterraggi(a:Aeroporto) extends Actor{
  private val aeroporto = a
  def receive = {
    case ChiediAtterraggio(p:Aereo) =>  aeroporto.arrivi += p
    									   if(aeroporto.ritardiInArrivi > 0){
    									     println("restano " + aeroporto.ritardiInArrivi + "ritardi in atterraggio")
    										   aeroporto.pista ! Atterra(p)
    										   aeroporto.removeRitardoInArrivo
    									   }
    case FaiAtterrare => if(aeroporto.arrivi.isEmpty){
    						println("nuovo ritardo in atterraggio")
    						aeroporto.addRitardoInArrivo
    						}
    					 else
    					   aeroporto.pista ! Atterra(aeroporto.arrivi.dequeue)
  }
}

class GestoreDecolli(a:Aeroporto) extends Actor{
  private val aeroporto = a
  def receive = {
    case ChiediDecollo(p:Aereo) => println("Accodo in decollo")
      										aeroporto.partenze += p
    									   if(aeroporto.ritardiInPartenza > 0){
    									     println("Parte subito")
    										   aeroporto.pista ! Decolla(p)
    										   aeroporto.removeRitardoInPartenza
    										   println("ci sono ancora " + aeroporto.ritardiInPartenza + "ritardi")
    									   }
    case FaiDecollare => if(aeroporto.partenze.isEmpty){
    						println("Nuovo ritardi in decollo")
    						aeroporto.addRitardoInPartenza
    						}		
    					 else
    						aeroporto.pista ! Decolla(aeroporto.partenze.dequeue)
  }
}

class Aeroporto(n:String){
  
	implicit val system = ActorSystem("planes")
	val timetable = ListBuffer[String]()
	
	private val _nome = n
	private val _partenze = Queue[Aereo]()
	private val _arrivi = Queue[Aereo]()
	private var _ritardiInPartenza = 0
	private var _ritardiInArrivi = 0
	
	def nome = _nome
	def partenze = _partenze 
	def arrivi = _arrivi
	def ritardiInPartenza = _ritardiInPartenza
	def ritardiInArrivi = _ritardiInArrivi
	
	def addRitardoInPartenza = {_ritardiInPartenza = _ritardiInPartenza + 1}
	def addRitardoInArrivo = {_ritardiInArrivi = _ritardiInArrivi + 1}
	def removeRitardoInPartenza = {_ritardiInPartenza = _ritardiInPartenza - 1}
	def removeRitardoInArrivo = {_ritardiInArrivi = _ritardiInArrivi - 1}
	
	
	
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