package funzionale_stash_per_console

import Reaper._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.ActorRef
import scala.language.implicitConversions
import com.typesafe.config.ConfigFactory

object Simulazione extends App {
import Names._
  //funzioni parziali  
  val checkIfDigit = (x: String) => x forall Character.isDigit

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

  def checkInput(x: Any) = (isInteger orElse isStringDigit orElse isCharDigit orElse otherTypes)(x)

  def checkParameters(args: Array[String], nAeroporti: => String, nAerei: => String /*, correctType: String=>Boolean*/ ): Boolean = {
    args.size match {
      case x if x < 2 =>
        println("Errore: Sono richiesti due parametri numerici")
        false
      case x if x >= 2 => (!(checkInput(nAeroporti)) || !checkInput(nAerei)) match {
        case true =>
          println("Errore: inseriti parametri non numerici")
          false
        case false => true
      }
    }

  }

  var i = 0
  var j = 0
  //closures
  def nomeAeroporto = {
    i = i + 1
    "Aeroporto" + i
  }
  def nomeAereo = {
    j = j + 1
    "Aereo" + j
  }

  def fallisci = println("La simulazione non può partire")

  def decolloAerei(aerei: List[Aereo], system : ActorSystem) = Future{
    aerei.par foreach { a => println(system.actorSelection(a.partenza.path + "/" + gestoreDecolli))
      system.actorSelection("/user/" + a.partenza.path.name + "/" + gestoreDecolli) ! ChiediDecollo(a)
      Thread.sleep(1500)
    }
  }
  
  def setTimetables(aerei: List[Aereo], aeroporti: List[ActorRef], reaper: ActorRef) = Future {
    //trait con modifiche impilabili (l'ordine conta!!!!), funzioni anonime passate come argomento di altre funzioni
    aeroporti map { a: ActorRef =>
      val tt = new PreparaTimetable(a) with StartWithDeparture with Normalizza with Randomize
      a ! setTimetable(tt.trasforma(tt.recInit((aerei filter (_.partenza.equals(a))).toList ++ (aerei filter (_.arrivo.equals(a))).toList)))
      reaper ! WatchMe(a)
    }
  }

  def attivazioneAeroporti(aeroporti: List[ActorRef]) = {
    aeroporti.par foreach (a => a ! Start)
  }



  def creaSistema(n1: String, n2: String) = {
    //conversioni implicite
    implicit def strToInt(x: String) = x.toInt
    implicit def IndexSeqToList[T](x: IndexedSeq[T]) = x.toList

/*val config = ConfigFactory.parseString("""
      akka{
        actor {
          queued-dispatcher {
    	      mailbox-type =”akka.dispatch.UnboundedDequeueBasedMailBox”
          }
        }
      }""")*/

    val config = ConfigFactory.parseString(
  """
  | akka.actor {
  |   default-dispatcher {
  |     mailbox-type = "akka.dispatch.UnboundedDequeBasedMailbox"
  |   }
  | }                                                                
  """.stripMargin)

    implicit val system = ActorSystem("planes",config)

    val reaper = system.actorOf(Props(new ProductionReaper))

    val nAeroporti: Int = n1 //conversione implicita
    val nAerei: Int = n2 //conversione implicita
    val aeroporti = 1 to nAeroporti map { _ =>
      val nome = nomeAeroporto
      system.actorOf(Props(new Aeroporto( nome)), name = nome)
    }

    println("NOME" + "\t" + "PARTENZA" + "\t" + "ARRIVO")
    val index = List.range(0, nAeroporti, 1)
    val aerei = 1 to nAerei map {
      _ =>
        val idx = Random.shuffle(index)
        //  j = j + 1
        val nome = nomeAereo
        println(nome + "\t" + aeroporti(idx(0)).path + "\t" + aeroporti(idx(1)).path)
        new Aereo(aeroporti(idx(0)), aeroporti(idx(1)), nome)
    }

    //prima aggiungo gli aerei alle code di partenza dei rispettivi aeroporti di partenza
    //e setto le timetable
    //fatto ciò, posso dare lo start agli aeroporti
    for {
      t <- setTimetables(aerei, aeroporti, reaper)
      a <- decolloAerei(aerei, system)} yield attivazioneAeroporti(aeroporti)


  }

//ritorna funzioni 
  checkParameters(args, args(0), args(1) /*, isAllDigits*/ ) match {
    case false => fallisci
    case true => creaSistema(args(0), args(1))
  } 
}
