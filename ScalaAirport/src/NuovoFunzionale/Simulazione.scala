package NuovoFunzionale

import Reaper._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.ActorRef
import scala.language.implicitConversions
import com.typesafe.config.ConfigFactory

/**
 * Genera il sistema di aeroporti.
 * Occorre dare in input il numero di aeroporti e aerei da creare.
 * Dopo la creazione di aerei, aeroporti e relative timetable, 
 * si ha l'attivazione di aerei e aeroporti.
 *
 * Ogni tabella oraria è costruita casualmente ma sempre con una D come primo elemento
 */
object Simulazione extends App {

   /**
   * Metodo che controlla se la stringa x è composta di sole cifre numeriche o no
   * 
   * @param		x	la stringa da controllare
   * @return	true	se la stringa è composta di sole cifre numeriche
   * 			false	altrimenti 	
   */
  val checkIfDigit = (x: String) => x forall Character.isDigit

  
  //funzioni parziali  
  
  //controlla se una stringa contiene solo cifre numeriche
  def isStringDigit: PartialFunction[Any, Boolean] = {
    case x: String => checkIfDigit(x)
  }

  //controlla se il carattere in input è una cifra numerica
  def isCharDigit: PartialFunction[Any, Boolean] = {
    case x: Char => x.isDigit
  }

  //controlla se ho inserito un intero
  def isInteger: PartialFunction[Any, Boolean] = {
    case x: Int => true
  }

  //controlla se ho inserito qualcosa
  def otherTypes: PartialFunction[Any, Boolean] = {
    case _ => false
  }

  //'concatenazione' di funzioni parziali per il check dell'input
  def checkInput(x: Any) = (isInteger orElse isStringDigit orElse isCharDigit orElse otherTypes)(x)

  /**
   * Metodo che si occupa di controllare che siano stati inseriti tutti gli inout corretti
   * richiesti dal programma
   * 
   * @param		args		array dei parametri inseriti dall'utente
   * @param		nAeroporti	numero di aeroporti inserito dall'utente 
   * 						(lazy: l'utente potrebbe non averlo inserito)
   * @param		nAerei		numero di aerei inserito dall'utente
   * 						(lazy: l'utente potrebbe non averlo inserito)
   */
  def checkParameters(args: Array[String], nAeroporti: => String, nAerei: => String ): Boolean = {
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

  /**
   * Future che si occupa di comunicare a ogni aeroporto di partenza di un aereo che 
   * l'a-esimo aereo è partito (uso dei loop paralleli)
   * 
   * @param		aerei	lista degli aerei da far decollare
   */
  def decolloAerei(aerei: List[Aereo]) = Future{
    aerei.par foreach { a =>
      a.partenza ! ChiediDecollo(a)
      Thread.sleep(1500)
    }
  }
  
  /**
   * Future che si occupa di:
   * 	- generare le timetable degli aeroporti randomizzate e che iniziano con una "D"
   *  	- comunicare all'attore reaper di osservare l'a-esimo aereo
   */
  def setTimetables(aerei: List[Aereo], aeroporti: List[ActorRef], reaper: ActorRef) = Future {
    //trait con modifiche impilabili (l'ordine conta!!!!), funzioni anonime passate come argomento di altre funzioni
    aeroporti map { a: ActorRef =>
      val tt = new PreparaTimetable(a) with StartWithDeparture with Normalizza with Randomize
      a ! setTimetable(tt.trasforma(tt.recInit((aerei filter (_.partenza.equals(a))).toList ++ (aerei filter (_.arrivo.equals(a))).toList)))
      reaper ! WatchMe(a)
    }
  }

  /**
   * Metodo che si occupa di comunicare a ogni aeroporto
   * che possono iniziare la loro attività
   * (uso di loop paralleli)
   */
  def attivazioneAeroporti(aeroporti: List[ActorRef]) = {
    aeroporti.par foreach (a => a ! Start)
  }


  /**
   * Dati gli input dell'utente, il metodo si occupa di generare le liste di aerei e aeroporti
   */
  def creaSistema(n1: String, n2: String) = {
    //conversioni implicite
    implicit def strToInt(x: String) = x.toInt
    implicit def IndexSeqToList[T](x: IndexedSeq[T]) = x.toList
    implicit val system = ActorSystem("planes")

    val reaper = system.actorOf(Props(new ProductionReaper))

    val nAeroporti: Int = n1 //conversione implicita
    val nAerei: Int = n2 //conversione implicita
    
    //generazione degli aereoporti
    val aeroporti = 1 to nAeroporti map { _ =>
      val nome = nomeAeroporto
      system.actorOf(Props(new Aeroporto( nome)), name = nome)
    }
    
   
    //generazione degli aerei
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
      a <- decolloAerei(aerei)} yield attivazioneAeroporti(aeroporti)


  }

  //equivalente del corpo del main
  checkParameters(args, args(0), args(1) ) match {//ritorna funzioni 
    case false => fallisci
    case true => creaSistema(args(0), args(1))
  } 
}
