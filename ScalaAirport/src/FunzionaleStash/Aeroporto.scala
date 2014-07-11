package FunzionaleStash

import scala.collection.mutable.Queue
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.postfixOps
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
import akka.util.Timeout

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
case object Done
case object Start
case class setTimetable(l: List[String])
case object Next
//sistema di attori, alcuni hanno un proprio stato interno



object Names { //nomi degli attori figli di aeroporto
  val pista = "pista"
  val gestoreDecolli = "richiestaDecollo"
  val gestoreAtterraggi = "richiestaAtterraggio"
  val gestoreRitardi = "gestoreRitardi"
}

/**
 * Classe che rappresenta un Aereo. 
 * 
 * @constructor	crea un aereo non mutabile
 * @param	_name		nome identificativo dell'aereo
 * @param	_partenza	nome dell'aeroporto di partenza
 * @param	_arrivo		nome dell'aeroporto di arrivo
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
 * Gli aerei sono ricevuti solo dall'aeroporto a cui la pista appartiente 
 * (quindi seguendo l'ordine scandito dalla tabella oraria a meno dei ritardi)
 * e dal gestore dei ritardi dell'aeroporto.
 * Tiene il conto di quanti decolli e atterraggi sono effettuati;
 * quando sono stati effettuati tutti quelli previsti, manda un messaggio di Stop all'aeroporto
 * 
 * @contructor	crea una pista per gli aerei in atterraggio/decollo
 * @param		nPartenze	numero di partenze totali da effettuare
 * @param		nArrivi		numero di arrivi totali da effettuare
 */
class Pista(nPartenze: Int, nArrivi: Int) extends Actor {
  private val partenze = nPartenze
  private val arrivi = nArrivi
  private var partiti = 0
  private var arrivati = 0
  def receive = {
    //l'aereo a deve deve decollare
    //ritardo è true se a è in ritardo
    case Decolla(a: Aereo, ritardo: Boolean) =>
      println(a.name + " decolla da " + context.parent.path + " (in ritardo? " + ritardo + ")")
      partiti = partiti + 1
      //se sono stati effettuati tutti gli atterraggi e tutti i decolli, allora l'aeroporto ha terminato il suo lavoro
      (partiti == partenze && arrivi == arrivati) match {
        case true => context.parent ! Stop
        case false =>
      }
      Thread.sleep(500) //occupa la pista
      //a deve atterrare
      context.system.actorFor(a.arrivo.path + "/richiestaAtterraggio") ! ChiediAtterraggio(a)

       //se l'aereo non era in ritardo, comunico all'aeroporto che il decollo è stato effettuato
      ritardo match {
        case false => context.parent ! Done
        case true =>
      }

    //l'aereo a deve deve atterrare
    //ritardo è true se a è in ritardo
    case Atterra(a: Aereo, ritardo: Boolean) =>
      println(a.name + " atterra a " + context.parent.path + " (in ritardo? " + ritardo + ")")

      Thread.sleep(500) //occupa la pista
      arrivati = arrivati + 1
       //se sono stati effettuati tutti gli atterraggi e tutti i decolli, allora l'aeroporto ha terminato il suo lavoro
      (partiti == partenze && arrivi == arrivati) match {
        case true => context.parent ! Stop
        case false =>
      }
       //se l'aereo non era in ritardo, comunico all'aeroporto che l'atterraggio è stato effettuato
      ritardo match {
        case false => context.parent ! Done
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
    case DecollaInRitardo(a: Aereo) => //comunico alla pista che l'aereo a deve decollare in ritardo
      println(a.name + " decolla in ritardo")
      context.actorSelection("../" + pista) ! Decolla(a, true)

    case AtterraInRitardo(a: Aereo) => //comunico alla pista che l'aereo a deve atterrare in ritardo
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

    case ChiediAtterraggio(a: Aereo) => //l'aereo a chiede di atterrare

      ritardiA match {
        case 0 =>			//se non ci sono arrivi in ritardo pendenti, aggiungo l'aereo alla coda...
          println(a.name + " in coda atterraggi")
          arrivi += a
        case x if x > 0 =>	//...altrimenti comunico al gestore ritardi che c'è un atterraggio in ritardo da gestire
          println(a.name + " riparte subito, mancano " + ritardiA + " ritardi")
          ritardiA = ritardiA - 1
          context.actorSelection("../" + gestoreRitardi) ! AtterraInRitardo(a)
      }

    case FaiAtterrare =>  //richiesta di inviare un aereo da atterrare

      Try(arrivi.dequeue) match {
        case Success(aereo) =>		//se la coda non è vuota, invio un aereo...
          println("ok atterraggio di  " + aereo.name)
          context.parent ! Some(aereo) 
        case Failure(f) =>			//...altrimenti 
          ritardiA = ritardiA + 1	//incremento numero di atterraggi in ritardo pendenti
          println("nessun aereo da far atterrare in " + context.parent.path + ", ritardi " + ritardiA)
          context.parent ! None		//e comunico all'aeroporto che non ci sono aerei da far atterrare al momento

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
    case ChiediDecollo(a: Aereo) => //l'aereo a chiede di decollare
      println(a.name + " chiede decollo")

      ritardi match {
        case x if x > 0 =>			//se ci sono partenze in ritardo pendenti, comunico al gestore ritardi che c'è un atterraggio in ritardo da gestire
          println(a.name + " riparte subito, mancano " + ritardi + " ritardi")
          ritardi = ritardi - 1
          context.actorSelection("../" + gestoreRitardi) ! DecollaInRitardo(a)
        case 0 => partenze += a	//se non ci sono partenze in ritardo pendenti, aggiungo l'aereo alla coda
      }
    case FaiDecollare => //richiesta di inviare un aereo da decollare

      Try(partenze.dequeue) match {
        case Success(aereo) => //se la coda non è vuota, invio un aereo...
          println("ok decollo di  " + aereo.name)
          context.parent ! Some(aereo)
        case Failure(f) =>		//...altrimenti 
          println("nessun aereo da far decollare in " + context.parent.path + ", ritardi " + ritardi)
          ritardi = ritardi + 1		//incremento numero di decolli in ritardo pendenti
          context.parent ! None		//e comunico all'aeroporto che non ci sono aerei da far decollare al momento

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
class Aeroporto(n: String) extends Actor with Stash {
  import Names._
  var timetable = List[String]()
  var nextTransit = 0
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

  def next: Receive = {
    case Next => timetable(nextTransit) match {
      case "A" =>
        richiestaAtterraggio ! FaiAtterrare
        context.become(waitForPlane, discardOld = false)
        unstashAll
      case "D" =>
        richiestaDecollo ! FaiDecollare
        context.become(waitForPlane, discardOld = false)
        unstashAll
      case _ => stash
    }
  }

  def waitForPlane: Receive = {
    case Some(p: Aereo) if timetable(nextTransit).equalsIgnoreCase("D") =>
      pista ! Decolla(p, false)
      context.become(waitForDone, discardOld = false)
      unstashAll
    case /*(p:Aereo, "A")*/ Some(p: Aereo) if timetable(nextTransit).equalsIgnoreCase("A") =>
      pista ! Atterra(p, false)
      context.become(waitForDone, discardOld = false)
      unstashAll
    case None =>
      nextTransit += 1
      nextTransit match {
        case x if x == timetable.size =>
          context.become(normal, discardOld = false)
          unstashAll
        case x if x < timetable.size =>
          context.become(next, discardOld = false)
          unstashAll
          Thread.sleep(2000)
          self ! Next
      }

    case _ => stash
  }

  def waitForDone: Receive = {
    case Done =>
      nextTransit += 1
      nextTransit match {
        case x if x == timetable.size =>
          context.become(normal, discardOld = false)
          unstashAll
        case x if x < timetable.size =>
          context.become(next, discardOld = false)
          unstashAll
          Thread.sleep(2000)
          self ! Next
      }
    case _ => stash
  }

  def normal: Receive = {
    case Stop => self ! PoisonPill
    case _ => stash
  }

  def receive = {
    case setTimetable(t) =>
      timetable = t
      println(self.path + "\t" + timetable)
      _pista = context.actorOf(Props(new Pista(timetable count (_.equalsIgnoreCase("D")), timetable count (_.equalsIgnoreCase("A")))), name = Names.pista)

    case Start => timetable.size match {
      case 0 => self ! PoisonPill
      case x if x > 0 =>
        context.become(next, discardOld = false)
        self ! Next
    }

    case _ => stash

  }

  override val supervisorStrategy = OneForOneStrategy() {
    case _: NullPointerException => Resume
    case _: Exception => Escalate
  }

}


  
 
