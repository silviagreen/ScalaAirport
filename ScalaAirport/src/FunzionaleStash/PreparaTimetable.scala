package FunzionaleStash

import scala.util.Random

import akka.actor.ActorRef


//estensione di una classe funzione
/**
 * Classe che si occupa di convertire una lista di aerei (che partono da/arrivano in un dato aeroporto)
 * in una tabella oraria, cioè in una lista di "A" e "D"
 */
class createTimetable[-ActorRef] extends Function2[ActorRef, (ActorRef, ActorRef), String] /*(T1, T2) => R*/ {
  def apply(x: ActorRef, y: (ActorRef, ActorRef)) = {

    x match {
      case y._1 => "D"
      case y._2 => "A"
    }

  }

}

/**
 * Classe che, dato un aeroporto, espone il metodo per generare la timetable
 * 
 * @constructor		aeroporto	l'aeroporto di cui si vuole costruire la timetable
 */
class PreparaTimetable(a: ActorRef) {
  val aeroporto = a

/* //versione iterativa dell'argoritmo di inizializzazione della timetable
  def init(l: List[Aereo]) = {
    val generateTimetable = new createTimetable
    var i = -1
    val timetable = 1 to l.size map { _ =>
      i = i + 1
      generateTimetable(a, (l(i).partenza, l(i).arrivo))
    }
    timetable.toList

  }
 */ 
  
/**
 * Metodo che si occupa di generare ricorsivamente la timetable
 */
  def recInit(rest: List[Aereo]): List[String] = {//ricorsione
    val generateTimetable = new createTimetable
    rest match {
      case Nil => Nil
      case head :: tail => generateTimetable(a, (head.partenza, head.arrivo)) :: recInit(tail)

    }
  }

  /**
   * Metodo che non applica alcuna trasformazione alla lista in input
   */
  def trasforma(l: List[String]) = l
  
}
//traits
//trait per normalizzare la timetable
trait Normalizza extends PreparaTimetable {

  /**
   * Metodo che si occupa di rendere maiuscole le stringhe della lista in input,
   * poi applica la trasformazione prevista da super
   */
  override def trasforma(l: List[String]) = {
    val ln = l map { _.toUpperCase() }
    super.trasforma(ln)

  }

}

//trait per randomizzare la timetable
trait Randomize extends PreparaTimetable {
  
   /**
   * Metodo che si occupa di randomizzare la lista in input,
   * poi applica la trasformazione prevista da super
   */
  override def trasforma(l: List[String]) = {
    val lr = Random.shuffle(l)
    super.trasforma(lr)

  }
}

//trait per far partire la timetable con una partenza
trait StartWithDeparture extends PreparaTimetable {
  
  //funzione currificata
  /*ITERATIVA
   * def swap[T](a:Array[T])(i:Int)(j:Int): Array[T] = {
    val t = a(i)
      a(i) = a(j)
      a(j) = t
      a
  }*/
  
  /**
   * Funzione currificata ricorsiva che ritorna una lista in output 
   * uguale a quella di input
   * ma con l'i-esimo e il j-esimo elemento scambiati
   * 
   * @param		l	lista di partenza
   * @param		i	indice del primo elemento da scambiare
   * @param		j	indice del secondo elemento da scambiare
   * @return	lista uguale a l ma con l'i-esimo e il j-esimo elemento scambiati
   */
  def swap[T](l:List[T])(i:Int)(j:Int): List[T] = {//FUNZIONALE
    val (l1, rest1) = l.splitAt(i - 1)
    val (l2, rest2) = rest1.drop(1).splitAt(j - 1)
    val l3 = rest2.drop(1)
     l1 ::: l(j) :: l2 ::: l(i) :: l3
  }
  


  /**
   * Metodo ricorsivo che si occupa di controllare se la lista in input 
   * è composta di sole "A" o no
   * 
   * @param		rest	lista di stringhe da controllare
   * @return	true	se rest contiene solo "A"
   * 			false	altrimenti
   */
  def allArrivals(rest: List[String]): Boolean = rest match {//ricorsione
    case head :: Nil => head match {
      case "A" => true
      case "D" => false
    }
    case Nil => false
    case head :: tail => head match {
      case "A" => allArrivals(tail)
      case "D" => false
    }

  }

   /**
   * Metodo che si occupa di ritornare una lista in output uguale a quella input l,
   * ma che inizi con una "D" (se l ne contiene almeno una)
   * poi applica la trasformazione prevista da super
   */
  override def trasforma(l: List[String]) = { 
    
    allArrivals(l) match {
      case true => super.trasforma(l)
      case false if l.size > 1 => l(0) match {
        case "D" => super.trasforma(l)
        case "A" =>//currificazione all'opera
          val currySwap = swap[String](l/*.toArray*/)(0)_
          val firstP = l.indexOf("D")
          super.trasforma(currySwap(firstP).toList)
      }
      case _ => super.trasforma(l)
    }

  }
}
