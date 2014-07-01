package NOFun2Meglio


import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import scala.concurrent.Future
import akka.actor.ActorSystem
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import akka.actor.ActorSystem
import akka.actor.{Actor, Props}
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.actorRef2Scala


class Aereo(p: Aeroporto2, a:Aeroporto2, n:String){
  
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

class GestoreAtterraggi(a:Aeroporto2) extends Actor{
  private val aeroporto = a
  private val _arrivi = Queue[Aereo]()
  private var _ritardiInArrivi = 0
  def arrivi = _arrivi
  def ritardiInArrivi = _ritardiInArrivi
  def addRitardoInArrivo = {_ritardiInArrivi = _ritardiInArrivi + 1}
  def removeRitardoInArrivo = {_ritardiInArrivi = _ritardiInArrivi - 1}
  
  def receive = {
    case ChiediAtterraggio(p:Aereo) =>  arrivi += p
    									if(ritardiInArrivi > 0){
    									     println("restano " + ritardiInArrivi + "ritardi in atterraggio")
    										 aeroporto.pista ! Atterra(p)
    										 removeRitardoInArrivo
    									}
    case FaiAtterrare => if(arrivi.isEmpty){
    						//println("nuovo ritardo in atterraggio")
    						addRitardoInArrivo
    						}
    					 else
    					   aeroporto.pista ! Atterra(arrivi.dequeue)
  }
}

class GestoreDecolli(a:Aeroporto2) extends Actor{
  private val aeroporto = a
  private val _partenze = Queue[Aereo]()
  def partenze = _partenze 
  
  private var _ritardiInPartenza = 0
  def ritardiInPartenza = _ritardiInPartenza
  def addRitardoInPartenza = {_ritardiInPartenza = _ritardiInPartenza + 1}
  def removeRitardoInPartenza = {_ritardiInPartenza = _ritardiInPartenza - 1}
  
  def receive = {
    case ChiediDecollo(p:Aereo) => //println("Accodo in decollo")
      								partenze += p
    								if(ritardiInPartenza > 0){
    									aeroporto.pista ! Decolla(p)
    									removeRitardoInPartenza }
    case FaiDecollare => if(partenze.isEmpty){
    						//println("Nuovo ritardi in decollo")
    						addRitardoInPartenza
    					}		
    					 else
    						aeroporto.pista ! Decolla(partenze.dequeue)
  }
}

class Aeroporto2(n:String){
  
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