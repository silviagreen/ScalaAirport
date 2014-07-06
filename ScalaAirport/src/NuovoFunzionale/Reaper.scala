package NuovoFunzionale

import akka.actor.{Actor, ActorRef, Terminated}
import scala.collection.mutable.ArrayBuffer
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy._
 
/**
 * L'attore reaper è stato creato per poter "spegnere" l'actor system una volta che tutti gli aeroporti hanno terminato il loro lavoro e sono stati terminati
 */
object Reaper {
  // messaggio che consente all'oggetto reaper di aggiungere un ActorRef da osservare
  case class WatchMe(ref: ActorRef)
}
 
abstract class Reaper extends Actor {
  import Reaper._
 
  // Tiene traccia degli attori sotto osservazione
  val watched = ArrayBuffer.empty[ActorRef]
 
  def allSoulsReaped(): Unit
 
  // Osserva se gli attori sotto osservazione terminano
  final def receive = {
    case WatchMe(ref) =>
      context.watch(ref)
      watched += ref
    case Terminated(ref) =>
      println(ref.path + " Terminato")
      watched -= ref
      if (watched.isEmpty) allSoulsReaped()
  }
  
   override val supervisorStrategy = OneForOneStrategy() {
    case _: NullPointerException => Resume
    case _: Exception => Escalate
  }
}

class ProductionReaper extends Reaper {
  // Shutdown
  def allSoulsReaped(): Unit = context.system.shutdown()
}