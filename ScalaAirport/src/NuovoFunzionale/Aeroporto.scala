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
import scala.language.postfixOps

//Messaggi con pattern matching
abstract class Messaggi
case class DecollaInRitardo(a: Aereo) extends Messaggi
case class AtterraInRitardo(a: Aereo) extends Messaggi
case class ChiediDecollo(a: Aereo) extends Messaggi
case class ChiediAtterraggio(a: Aereo) extends Messaggi
case object FaiDecollare extends Messaggi
case object FaiAtterrare extends Messaggi
case class Decolla(a: Aereo, ritardo: Boolean) extends Messaggi
case class Atterra(a: Aereo, ritardo: Boolean) extends Messaggi
case class Done
case object Start
case class setTimetable(l: List[String])

object Names {
  val pista = "pista"
  val gestoreDecolli = "richiestaDecollo"
  val gestoreAtterraggi = "richiestaAtterraggio"
  val gestoreRitardi = "gestoreRitardi"
}

//sistema di attori, alcuni hanno un proprio stato interno

/**
 * Classe che rappresenta un Aereo. Ogni aereo ha:
 * 		- nome
 *   	- aeroporto di partenza
 *    	- aeroporto di arrivo
 */
class Aereo(p: ActorRef, a: ActorRef, n: String) {

  private val _name = n
  private val _partenza = p
  private val _arrivo = a

  def name = _name
  def partenza = _partenza
  def arrivo = _arrivo
}

/**
 * Attore che fa atterrare e decollare gli aerei uno per volta, in base all'ordine di arrivo.
 * Gli aerei sono ricevuti solo dall'aeroporto a cui la pista appartiente (quindi seguendo l'ordine scandito dalla tabella oraria a meno dei ritardi)
 * e dal gestore dei ritardi dell'aeroporto.
 * Tiene il conto di quanti decolli e atterraggi sono effettuati;
 * quando sono stati effettuati tutti quelli previsti, manda un messaggio di Stop all'aeroporto
 */
class Pista(nPartenze: Int, nArrivi: Int) extends Actor {
  private val partenze = nPartenze
  private val arrivi = nArrivi
  private var partiti = 0
  private var arrivati = 0
  def receive = {
    case Decolla(a: Aereo, ritardo: Boolean) =>
      val mittente = sender
      println(a.name + " decolla da " + context.parent.path + " (in ritardo? " + ritardo + ")")

      partiti = partiti + 1
      (partiti == partenze && arrivi == arrivati) match {
        case true => context.parent ! Stop
        case false =>
      }
      Thread.sleep(500) //occupa la pista
      (a.arrivo) ! ChiediAtterraggio(a)

      ritardo match {
        case false => mittente ! "Done"
        case true =>
      }

    case Atterra(a: Aereo, ritardo: Boolean) =>
      val mittente = sender
      println(a.name + " atterra a " + context.parent.path + " (in ritardo? " + ritardo + ")")

      Thread.sleep(500) //occupa la pista
      arrivati = arrivati + 1
      (partiti == partenze && arrivi == arrivati) match {
        case true => context.parent ! Stop
        case false =>
      }
      ritardo match {
        case false => mittente ! "Done"
        case true =>
      }
  }

  override val supervisorStrategy = OneForOneStrategy() {
    case _: NullPointerException => Resume
    case _: Exception => Escalate
  }

}

/**
 * Attore che deve riceve gli aerei arrivati/partiti in ritardo e li accoda per l'utilizzo della pista
 */
class GestoreRitardi extends Actor {
  import Names._
  def receive = {
    case DecollaInRitardo(a: Aereo) =>
      println(a.name + " decolla in ritardo")
      context.actorSelection("../" + pista) ! Decolla(a, true)

    case AtterraInRitardo(a: Aereo) =>
      println(a.name + " atterra in ritardo")
      context.actorSelection("../" + pista) ! Atterra(a, true)
  }

  override val supervisorStrategy = OneForOneStrategy() {
    case _: NullPointerException => Resume
    case _: Exception => Escalate
  }

}

/**
 * Attore che si occupa di gestire la coda degli aerei in arrivo.
 *
 * Mantiene uno stato interno:
 * 	-coda degli aerei in arrivo
 *  -numero di ritardi negli arrivi
 */
class GestoreAtterraggi extends Actor {

  import Names._
  private val arrivi = Queue[Aereo]()
  private var ritardiA = 0

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
          context.actorSelection("../" + gestoreRitardi) ! AtterraInRitardo(a)
      }

    case FaiAtterrare =>
      val mittente = sender
      Try(arrivi.dequeue) match {
        case Success(aereo) =>
          println("ok atterraggio di  " + aereo.name)
          mittente ! Some(aereo)
        case Failure(f) =>
          ritardiA = ritardiA + 1
          println("nessun aereo da far atterrare in " + context.parent.path + ", ritardi " + ritardiA)
          mittente ! None

      }

  }

  override val supervisorStrategy = OneForOneStrategy() {
    case _: NullPointerException => Resume
    case _: Exception => Escalate
  }

}

/**
 * Attore che si occupa di gestire la coda degli aerei in partenza.
 *
 * Mantiene uno stato interno:
 * 	-coda degli aerei in partenza
 *  -numero di ritardi negli partenza
 */
class GestoreDecolli extends Actor {
  import Names._
  private val partenze = Queue[Aereo]()
  private var ritardi = 0

  def receive = {
    case ChiediDecollo(a: Aereo) =>
      println(a.name + " chiede decollo")
      val mittente = sender
      ritardi match {
        case x if x > 0 =>
          println(a.name + " riparte subito, mancano " + ritardi + " ritardi")
          ritardi = ritardi - 1
          context.actorSelection("../" + gestoreRitardi) ! DecollaInRitardo(a)
        case 0 => partenze += a
      }
    case FaiDecollare =>
      val mittente = sender
      Try(partenze.dequeue) match {
        case Success(aereo) =>
          println("ok decollo di  " + aereo.name)
          mittente ! Some(aereo)
        case Failure(f) =>
          println("nessun aereo da far decollare in " + context.parent.path + ", ritardi " + ritardi)
          ritardi = ritardi + 1
          mittente ! None
      }
  }

  override val supervisorStrategy = OneForOneStrategy() {
    case _: NullPointerException => Resume
    case _: Exception => Escalate
  }

}

/**
 * Attore che rappresenta un aeroporto.
 * Ha 4 attori figli:
 * 	-pista
 *  -gestore dei ritardi
 *  -gestore della coda degli arrivi
 *  -gestore della coda delle partenze
 *
 * Ogni attore ha una tabella oraria (settata prima dello start dell'attore) e
 * delega la gestione dei decolli e atterraggi degli aerei ai figli.
 * Sucessivamente riceve il messaggio di Start e fa partire un Future che legge la tabella oraria e regola i decolli e gli atterraggi in base a questa
 * Quando riceve il messagio di Stop, l'attore termina
 */
class Aeroporto(n: String) extends Actor {
  import Names._
  var timetable = List[String]()

  private val _nome = n
  def nome = _nome

  private var _pista: ActorRef = _
  private val _richiestaDecollo = context.actorOf(Props(new GestoreDecolli), name = gestoreDecolli)
  private val _richiestaAtterraggio = context.actorOf(Props(new GestoreAtterraggi), name = gestoreAtterraggi)
  private val _gestoreRitardi = context.actorOf(Props(new GestoreRitardi), name = gestoreRitardi)

  def pista = _pista
  def richiestaDecollo = _richiestaDecollo
  def richiestaAtterraggio = _richiestaAtterraggio
  def GestoreRitardi = _gestoreRitardi

  def timetable_(l: List[String]) = timetable = l

  def proxTransito: Future[Unit] = Future {
    _pista = context.actorOf(Props(new Pista(timetable count (_.equalsIgnoreCase("D")), timetable count (_.equalsIgnoreCase("A")))), name = Names.pista)
    implicit val timeout = Timeout(100 seconds)
    timetable.size match {
      case 0 => self ! Stop
      case x if x > 0 =>
        for (t <- timetable) yield {
          t match {
            case "D" =>
              val future = richiestaDecollo ? FaiDecollare
              val result = Await.result(future, timeout.duration).asInstanceOf[Option[Aereo]]
              result match {
                case Some(a) =>
                  val future2 = pista ? Decolla(a, false)
                  val result = Await.result(future2, timeout.duration).asInstanceOf[String]
                  result match {
                    case _ =>
                  }
                case None =>
              }
              Thread.sleep(2000) //attendo che il volo arrivi
            case "A" =>
              val future = richiestaAtterraggio ? FaiAtterrare
              val results = Await.result(future, timeout.duration).asInstanceOf[Option[Aereo]]
              results match {
                case Some(a) =>

                  val future3 = pista ? Atterra(a, false)
                  val result = Await.result(future3, timeout.duration).asInstanceOf[String]
                  result match {
                    case _ => 
                  }
                case None =>
              }
              Thread.sleep(2000) //attendo che il volo arrivi

          }

        }
    }

  }

  def receive = {
    case Start => proxTransito
    case ChiediAtterraggio(p) => richiestaAtterraggio ! ChiediAtterraggio(p)
    case ChiediDecollo(p) => richiestaDecollo ! ChiediDecollo(p)
    case Stop => self ! PoisonPill
    case setTimetable(t) =>
      timetable = t
      println(self.path + "\t" + timetable)
  }

  override val supervisorStrategy = OneForOneStrategy() {
    case _: NullPointerException => Resume
    case _: Exception => Escalate
  }

}


  
 
