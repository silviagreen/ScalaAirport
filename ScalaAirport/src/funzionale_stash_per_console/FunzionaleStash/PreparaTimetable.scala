package funzionale_stash_per_console

import scala.util.Random

import akka.actor.ActorRef


//estensione di una classe funzione
class createTimetable[-ActorRef] extends Function2[ActorRef, (ActorRef, ActorRef), String] /*(T1, T2) => R*/ {
  def apply(x: ActorRef, y: (ActorRef, ActorRef)) = {

    x match {
      case y._1 => "D"
      case y._2 => "A"
    }

  }

}

//traits
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
  
//ricorsione
  def recInit(rest: List[Aereo]): List[String] = {
    val generateTimetable = new createTimetable
    rest match {
      case Nil => Nil
      case head :: tail => generateTimetable(a, (head.partenza, head.arrivo)) :: recInit(tail)

    }
  }

  def trasforma(l: List[String]) = l
  

}

//trait per normalizzare la timetable
trait Normalizza extends PreparaTimetable {

  override def trasforma(l: List[String]) = {
    val ln = l map { _.toUpperCase() }
    super.trasforma(ln)

  }

}

//trait per randomizzare la timetable
trait Randomize extends PreparaTimetable {
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
  
  //FUNZIONALE
  def swap[T](l:List[T])(i:Int)(j:Int): List[T] = {
    val (l1, rest1) = l.splitAt(i - 1)
    val (l2, rest2) = rest1.drop(1).splitAt(j - 1)
    val l3 = rest2.drop(1)
     l1 ::: l(j) :: l2 ::: l(i) :: l3
  }
  


  //ricorsione
  def allArrivals(rest: List[String]): Boolean = rest match {
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
