package NuovoFunzionale

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
case class CodeTerminate
case class Start
case class setTimetable(l:List[String])

class Aereo(p: ActorRef, a: ActorRef, n: String) {

  private val _name = n
  private val _partenza = p
  private val _arrivo = a

  def name = _name
  def partenza = _partenza
  def arrivo = _arrivo
}

class Pista(nPartenze : Int, nArrivi:Int, actor:ActorRef) extends Actor {
//  private val aeroporto = ae
  private val partenze = nPartenze//ae.timetable count (_.equalsIgnoreCase("D"))
  private val arrivi = nArrivi//ae.timetable count (_.equalsIgnoreCase("A"))
  private var partiti = 0
  private var arrivati = 0
  def receive = {
    //case UsaPista(message : String) => println(message)
    case Decolla(a: Aereo, ritardo: Boolean) =>
      println(a.name + " decolla da " + /*aeroporto.nome*/actor.path + " (in ritardo? " + ritardo + ")" /* + " " + sender*/ )
      partiti = partiti + 1
      if (partiti == partenze && arrivi == arrivati) {
       // println("FINE " + actor.path)
        context.parent ! Stop
      }
      Thread.sleep(500)//occupa la pista per 1 secondo
     // a.arrivo.richiestaAtterraggio ! ChiediAtterraggio(a)
      (a.arrivo) ! ChiediAtterraggio(a)

    case Atterra(a: Aereo, ritardo: Boolean) =>
      println(a.name + " atterra a " + actor.path + " (in ritardo? " + ritardo + ")" /* + " " + sender*/ )
      Thread.sleep(500) //occupa la pista per 1 secondo
      arrivati = arrivati + 1
      if (partiti == partenze && arrivi == arrivati) {
       // println("FINE " + actor.path)
        context.parent ! Stop
      }
  }

  override val supervisorStrategy = OneForOneStrategy() {
    case _: NullPointerException => Resume
    case _: Exception => Escalate
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
    case DecollaInRitardo(a: Aereo) =>
      println(a.name + " decolla in ritardo")
      context.actorSelection("../pista") ! Decolla(a, true) //UsaPista(a.name + " decolla in ritardo da " + aeroporto.nome)

    case AtterraInRitardo(a: Aereo) =>
      println(a.name + " atterra in ritardo")
      context.actorSelection("../pista") ! Atterra(a, true) //UsaPista(a.name + " atterra in ritardo a " + aeroporto.nome)
  }

  override val supervisorStrategy = OneForOneStrategy() {
    case _: NullPointerException => Resume
    case _: Exception => Escalate
  }

  // override def postStop { println("TestActor::postStop ritardi") }
}

class GestoreAtterraggi(a: ActorRef) extends Actor {

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
        case x if x > 0 =>
          println(a.name + " riparte subito, mancano " + ritardiA + " ritardi")
          ritardiA = ritardiA - 1
          //(aeroporto.pista) ! UsaPista(a.name + " atterra a " + a.arrivo.nome)
          context.actorSelection("../gestoreRitardi") ! AtterraInRitardo(a)
        // decolli = decolli + 1
      }

    case FaiAtterrare =>
      val mittente = sender
      Try(arrivi.dequeue) match {
        case Success(aereo) =>
          println("ok atterraggio di  " + aereo.name)
          mittente ! Some(aereo)
        case Failure(f) =>
          ritardiA = ritardiA + 1
          println("nessun aereo da far atterrare in " + a.path +", ritardi " + ritardiA)
          mittente ! None

      }

  }

  override val supervisorStrategy = OneForOneStrategy() {
    case _: NullPointerException => Resume
    case _: Exception => Escalate
  }

  // override def postStop { println("TestActor::postStop atterraggi") }
}

class GestoreDecolli(a: ActorRef) extends Actor {
  private val aeroporto = a
  private var partenze = Queue[Aereo]()
  private var ritardi = 0

  def receive = {
    case ChiediDecollo(a: Aereo) =>
      println(a.name + " chiede decollo")
      val mittente = sender
      ritardi match {
        case x if x > 0 =>
          println(a.name + " riparte subito, mancano " + ritardi + " ritardi")
          ritardi = ritardi - 1
          //aeroporto.pista ! UsaPista(a.name + " decolla da " + a.partenza.nome)
          context.actorSelection("../gestoreRitardi") ! DecollaInRitardo(a)
        //a.arrivo.richiestaAtterraggio ! ChiediAtterraggio(a)

        case 0 => partenze += a
      }
    case FaiDecollare =>
      val mittente = sender

      Try(partenze.dequeue) match {
        case Success(aereo) =>this
          println("ok decollo di  " + aereo.name)
          mittente ! Some(aereo)
        case Failure(f) =>
          println("nessun aereo da far decollare in " + a.path + ", ritardi " + ritardi)
          ritardi = ritardi + 1
          mittente ! None
      }
  }

  override val supervisorStrategy = OneForOneStrategy() {
    case _: NullPointerException => Resume
    case _: Exception => Escalate
  }

  // override def postStop { println("TestActor::postStop decolli") }
}

class Aeroporto(n: String) extends Actor{

  var timetable = List[String]()

  private val _nome = n
  def nome = _nome

  private var _pista: ActorRef = _
  private val _richiestaDecollo = context.actorOf(Props(new GestoreDecolli(self)), name = "richiestaDecollo")
  private val _richiestaAtterraggio = context.actorOf(Props(new GestoreAtterraggi(self)), name = "richiestaAtterraggio")
  private val _gestoreRitardi = context.actorOf(Props(new GestoreRitardi(this)), name = "gestoreRitardi")
  //private val _manager: ActorRef = system.actorOf(Props(new Manager(system)), name = "manager")

  def pista = _pista
  def richiestaDecollo = _richiestaDecollo
  def richiestaAtterraggio = _richiestaAtterraggio
  def GestoreRitardi = _gestoreRitardi
  //def manager = _manager

  def timetable_(l: List[String]) = timetable = l

  def proxTransito: Future[Unit] = Future {
    _pista = context.actorOf(Props(new Pista(timetable count (_.equalsIgnoreCase("D")), timetable count (_.equalsIgnoreCase("A")) , self)), name = "pista")
    implicit val timeout = Timeout(100 seconds)
    timetable.size match {
      case 0 => self ! Stop//manager ! CodeTerminate
      case x if x > 0 =>
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
           Thread.sleep(2000)//attendo che il volo arrivi
        case "A" =>
          val future = richiestaAtterraggio ? FaiAtterrare
          val results = Await.result(future, timeout.duration).asInstanceOf[Option[Aereo]]
          results match {
            case Some(a) => pista ! Atterra(a, false) //UsaPista(a.name + " atterra da " + a.arrivo.nome)
            case None =>
          }
          Thread.sleep(2000)//attendo che il volo arrivi

      }

    }
    }
    

  }
  
  def receive = {
    case Start => proxTransito
    case ChiediAtterraggio(p) => richiestaAtterraggio ! ChiediAtterraggio(p)
    case ChiediDecollo(p) => richiestaDecollo ! ChiediDecollo(p)
    case Stop => self ! PoisonPill
    case setTimetable(t) => timetable = t
    						println(self.path + "\t" + timetable)
  }



}


  
 
