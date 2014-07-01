package FunPlusSembraOOk

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

object Simulazione extends App{


  /*class IntToStringConverter extends Int => String {
  def apply(i: Int): String = i.toString
}*/
  
  val checkIfDigit = (x:String) => x forall Character.isDigit

  def isAllDigit: PartialFunction[String, Boolean] = {
    case x: String => checkIfDigit(x)
  }
  

  def checkParameters(args: Array[String], nAeroporti: => String, nAerei: => String /*, correctType: String=>Boolean*/ ): Boolean = {
    args.size match {
      case x if x < 2 => println("Errore: Sono richiesti due parametri numerici")
    		  			false
      case x if x >= 2 => (!isAllDigit(nAeroporti) || !isAllDigit(nAerei)) match {
        case true => println("Errore: inseriti parametri non numerici")
        			 false
        case false => true
      }
    }

  
  }

  var i = 0
  var j = 0

  def fallisci = println("La simulazione non può partire")
  
  def decolloAerei(aerei : List[Aereo]) = Future {
    aerei foreach {a => a.partenza.richiestaDecollo ! ChiediDecollo(a) 
      					Thread.sleep(1500)}
  }
  
  def attivazioneAeroporti (aeroporti : List[Aeroporto]) = Future {
        aeroporti foreach ( a => a.start)
  }

  def creaSistema(n1: String, n2: String) = {

    implicit def strToInt(x: String) = x.toInt
    implicit def IndexSeqToList[T](x: IndexedSeq[T]) = x.toList
    
    val nAeroporti: Int = n1 //.toInt
    val nAerei: Int = n2 //.toInt
    val aeroporti = 1 to nAeroporti map { _ =>
      i = i + 1
      new Aeroporto("Aeroporto" + (i))

    }

    println("NOME"+ "\t" + "PARTENZA" + "\t" + "ARRIVO")
    val index = List.range(0, nAeroporti, 1)
    val aerei = 1 to nAerei map {
      _ =>
        val idx = Random.shuffle(index)
        j = j + 1
        println("aereo" + j + "\t" + aeroporti(idx(0)).nome + "\t" + aeroporti(idx(1)).nome)
        new Aereo(aeroporti(idx(0)), aeroporti(idx(1)), "aereo" + j)
    }

    aeroporti map { a: Aeroporto =>
      val tt = new PreparaTimetable(a) //with StartWithDeparture with Normalizza with Randomize
      a.timetable = tt.trasforma(tt.recInit((aerei filter (_.partenza.equals(a))).toList ++ (aerei filter (_.arrivo.equals(a))).toList))
    }


    println("\n")
    aeroporti foreach (a => println(a.nome + "\t" + a.timetable))
    println("\n")

    println(System.nanoTime())
    
    val decolli = decolloAerei(aerei)
    val attivazione = attivazioneAeroporti(aeroporti)

  
    for{f <- decolli
        g <- attivazione}
      yield println("Attivazione completata")//System.out.println("Number of active threads from the given thread: " + Thread.activeCount()); // println("Creazione Completata")
    



    //11.011812466 Seconds

  }

//  def main(args: Array[String]): Unit = {

    checkParameters(args, args(0), args(1) /*, isAllDigits*/ ) match {
      case false => fallisci
      case true => creaSistema(args(0), args(1))

    }
  //}
}

  /*aeroporti foreach ( a =>
    	  		a.timetable = Random.shuffle(List.tabulate((aerei filter(_.arrivo.equals(a))).size )(n => "A") ++ List.tabulate((aerei filter(_.partenza.equals(a))).size)(a => "D"))
    	    )*/