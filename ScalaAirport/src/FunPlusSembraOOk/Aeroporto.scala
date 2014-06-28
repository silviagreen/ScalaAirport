package FunPlusSembraOOk

import scala.collection.mutable.Queue
import scala.concurrent._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import akka.actor._
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.Stash
import akka.pattern.ask
import akka.pattern.pipe
import akka.util.Timeout
import akka.util.Timeout



class Aereo(p: Aeroporto, a:Aeroporto, n:String){
  
  private val _name = n
  private val _partenza = p
  private val _arrivo = a
  
  def name = _name
  def partenza = _partenza
  def arrivo = _arrivo
}

 

//Messaggi
abstract class Messaggi
case class Decolla(a:Aereo) extends Messaggi{
  val aereo = a
}
case class Atterra(a:Aereo) extends Messaggi{
  val aereo = a
}
case class ChiediDecollo(a:Aereo) extends Messaggi
case class ChiediAtterraggio(a:Aereo) extends Messaggi
case object FaiDecollare extends Messaggi
case object FaiAtterrare extends Messaggi
case class UsaPista(m:String) extends Messaggi

class Pista extends Actor{
  def receive = {
    case UsaPista(message : String) => println(message)
  }
  
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

class GestoreAtterraggi(a:Aeroporto) extends Actor{
  
   
  
  private val aeroporto = a
  private var arrivi = Queue[Aereo]()
  private var ritardiA = 0
  
  def receive = {								   
    case ChiediAtterraggio(a:Aereo) => val mittente = sender//println(ritardiA + " in " + aeroporto.nome)
    ritardiA match {
      case 0 => //println("accoda " + a.name + " in " + aeroporto.nome)
        arrivi += a
      case x if x > 0 =>	//println("c'è un ritardo " + a.name + " parte subito")
      						(aeroporto.pista) ! UsaPista(a.name + " atterra a " + a.partenza.nome)
      
    		  				ritardiA = ritardiA - 1
      
    }
    case FaiAtterrare => 
       val mittente = sender
    Try(arrivi.dequeue) match{
      case Success(aereo) => //println(aereo.name + "atterra")
      mittente ! Some(aereo)
      case Failure(f) => 
        ritardiA = ritardiA + 1
        println("ritardo in " + aeroporto.nome +" " + ritardiA)
        mittente ! None
        
    }
  }
}

class GestoreDecolli(a:Aeroporto) extends Actor{
  private val aeroporto = a
  private var partenze = Queue[Aereo]()
  private var ritardi = 0
  
  def receive = { 										
    case ChiediDecollo(a:Aereo) =>	val mittente = sender
    ritardi match {
      case x if x > 0 =>	aeroporto.pista ! UsaPista(a.name + " decolla da " + a.partenza.nome)
    		  				a.arrivo.richiestaAtterraggio ! ChiediAtterraggio(a)
    		  				ritardi = ritardi - 1
    		  				
      case 0 =>	partenze += a
    }
     case FaiDecollare => val mittente = sender
     
     Try(partenze.dequeue) match{
      case Success(aereo) => 
        mittente ! Some(aereo)
      case Failure(f) => 
        ritardi = ritardi + 1
        mittente ! None
    }
  }
}

class Aeroporto(n:String){
  
	implicit val system = ActorSystem("planes")
	var timetable = List[String]()
	
	private val _nome = n
	def nome = _nome

	private val _pista = system.actorOf(Props(new Pista), name = "pista")
	private val _richiestaDecollo = system.actorOf(Props(new GestoreDecolli(this)), name = "richiestaDecollo")
	private val _richiestaAtterraggio = system.actorOf(Props(new GestoreAtterraggi(this)), name = "richiestaAtterraggio")
	
	def pista = _pista
	def richiestaDecollo = _richiestaDecollo
	def richiestaAtterraggio = _richiestaAtterraggio
	
	def timetable_(l:List[String]) = timetable = l
	
	def proxTransito : Future[Unit] = Future {
	  implicit val timeout = Timeout(10 seconds)
	  timetable foreach { t =>
	    t match {
	    case "D" => 
	    			
	    			  val future = richiestaDecollo ? FaiDecollare
	    			  val result = Await.result(future, timeout.duration).asInstanceOf[Option[Aereo]]
	    			result match {
	    				case Some(a) => pista ! UsaPista(a.name + " decolla da " + a.partenza.nome)
	    				//println("dopo pista")
	    				//println("chiedo di accodare " + a.name)
	    								a.arrivo.richiestaAtterraggio ! ChiediAtterraggio(a)
	    				case None => 
	    			}
	    			
	    			/*val futureAereo = ask(richiestaDecollo, FaiDecollare).mapTo[Option[Aereo]]
	    			futureAereo.onSuccess{
	    			  case result => result match {
	    			    case Some(a) => pista ! UsaPista(a.name + " decolla da " + a.partenza.nome)
	    				println("chiedo di accodare " + a.name)
	    								a.arrivo.richiestaAtterraggio ! ChiediAtterraggio(a)
	    				case None => 
	    			  }
	    			}*/
	    			
	    			
	    case "A" => val future = richiestaAtterraggio ? FaiAtterrare
	    			val results = Await.result(future, timeout.duration).asInstanceOf[Option[Aereo]]
	    			results match {
	    				case Some(a) => pista ! UsaPista(a.name + " atterra da " + a.arrivo.nome)
	    				case None => 
	    			}
	    			
	  }
	    
	  }
	}
	
	def start = proxTransito
	
}

 
