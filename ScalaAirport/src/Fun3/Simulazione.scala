package Fun3

import scala.util.Random



object Simulazione {
  
  def isAllDigits(x: String) = x forall Character.isDigit
    
  def checkParameters(args: Array[String], nAeroporti: => String, nAerei: => String, correctType: String=>Boolean): Boolean = {
    if(args.size < 2){
      println("Errore: Sono richiesti due parametri numerici")
      false
    }
    else{
      if(!correctType(nAeroporti) || !correctType(nAerei)){
        println("Errore: inseriti parametri non numerici")
        false
      }else true
    }
  }
  
  var i = 0
  var j = 0

  def main(args: Array[String]): Unit = {
   
    
    if(checkParameters(args, args(0), args(1), isAllDigits)){
     val nAeroporti = args(0).toInt
    val nAerei = args(1).toInt
    val aeroporti =  1 to nAeroporti map{ _=> i = i + 1 
      										new Aeroporto("Aeroporto" + (i))}
    
   /* for(i <- aeroporti)
      println(i.nome)*/
    
    var index = List.range(0, nAeroporti, 1)
    println("NOME\t PARTENZA\t ARRIVO")
    val aerei = 1 to nAerei map{
      _ => index = Random.shuffle(index)
    	   j = j + 1
    	   aeroporti(index(0)).timetable += "D"
    	   aeroporti(index(1)).timetable += "A"
    	   println("aereo" + j + "\t" + aeroporti(index(0)).nome + "\t" + aeroporti(index(1)).nome)
    	   new Aereo(aeroporti(index(0)), aeroporti(index(1)), "aereo" + j)  
    }
    aeroporti foreach (a => a.start)
    aerei foreach (a => a.partenza.richiestaDecollo ! ChiediDecollo(a)/*a.partenza.richiestaAtterraggio(a)*/)
  
  }else{
    println("La simulazione non può partire")
  }
  }
}