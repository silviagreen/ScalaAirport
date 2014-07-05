package NuovoFunzionale

import scala.util.Random

import akka.actor.ActorRef


//estensione di una classe funzione
class createTimetable[-ActorRef, -Aereo] extends Function2[ActorRef, (Aereo, Aereo), String] /*(T1, T2) => R*/ {
  def apply(x: ActorRef, y: (Aereo, Aereo)) = {

    x match {
      case y._1 => "D"
      case y._2 => "A"
    }

  }

}
/*
class createTimetable extends Function2[Array[Array[Int]], Int, List[String]] {
  def apply(matrix: Array[Array[Int]], airport:Int) = {
    0 to matrix(airport).size match {
      case x if x != 0 => 
    }
  }
}*/

//traits

class PreparaTimetable(a: ActorRef) {
  val aeroporto = a



  def init(l: List[Aereo]) = {
    val generateTimetable = new createTimetable
    var i = -1
    val timetable = 1 to l.size map { _ =>
      i = i + 1
      generateTimetable(a, (l(i).partenza, l(i).arrivo))
    }

    timetable.toList
    // stringlist_(List.tabulate(arrivi.size)(i => "A") ++ List.tabulate(partenze.size)(i => "D"))

  }
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

trait Normalizza extends PreparaTimetable {

  override def trasforma(l: List[String]) = {
    val ln = l map { _.toUpperCase() }
    super.trasforma(ln)

  }

}

trait Randomize extends PreparaTimetable {
  override def trasforma(l: List[String]) = {
    val lr = Random.shuffle(l)
    //println("randomizzo");
    super.trasforma(lr)

  }
}

trait StartWithDeparture extends PreparaTimetable {
  //funzione currificata
  def swap[T](a:Array[T])(i:Int)(j:Int): Array[T] = {
    val t = a(i)
      a(i) = a(j)
      a(j) = t
      a
  }

 /* def swap[T](a: Array[T], i: Int, j: Int): Array[T] =
    {
      val t = a(i)
      a(i) = a(j)
      a(j) = t
      a
    }*/

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

  override def trasforma(l: List[String]) = { //println("con D all'inizio")
    l.toArray
    allArrivals(l) match {
      case true => super.trasforma(l)
      case false if l.size > 1 => l(0) match {
        case "D" => super.trasforma(l)
        case "A" =>//currificazione all'opera
          val currySwap = swap[String](l.toArray)(0)_
          val firstP = l.indexOf("D")
          super.trasforma(currySwap(firstP).toList)
          //super.trasforma(swap[String](l.toArray, 0, firstP).toList)
      }
      case _ => super.trasforma(l)
    }

  }
}
