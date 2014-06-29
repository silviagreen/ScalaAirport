package FunPlusSembraOOk

import scala.util.Random


  class createTimetable[-Aeroporto, -Aereo] extends Function2[Aeroporto, (Aereo, Aereo), String]/*(T1, T2) => R*/ {
  	def apply(x: Aeroporto, y: (Aereo, Aereo)) = {
  	  
  	    		x match {
  	    		  case y._1 => "D"
  	    		  case y._2 => "A"
  	    		}
  	    			
  	    
  	}
  
}


class PreparaTimetable (a:Aeroporto, l:List[Aereo]) {
  val aeroporto = a

   var _stringList: List[String] = recInit(l)//init//
 
    def stringList = _stringList
 
    def stringList_=(stringList: List[String]) = _stringList = stringList
    
     
  
  def init(/*l:List[Aereo]*/) = {
   // val (arrivi, partenze) = l partition (_.arrivo.equals(aeroporto))
    val generateTimetable = new createTimetable
    var i = -1
    val timetable = 1 to l.size map{ _=> i = i + 1 
        								generateTimetable(a, (l(i).partenza, l(i).arrivo) ) }
    
    
    timetable.toList
   // stringlist_(List.tabulate(arrivi.size)(i => "A") ++ List.tabulate(partenze.size)(i => "D"))
    
  }
  
  def recInit(rest:List[Aereo]) : List[String] = {
    val generateTimetable = new createTimetable
    println(rest.size);
    rest match {
       case Nil => Nil
      case head :: tail => generateTimetable(a, (head.partenza, head.arrivo) ) :: recInit(tail)
     
    }
  }
  
  def trasforma() = stringList
  
  
}

trait Normalizza extends PreparaTimetable{
  
  

	abstract override def trasforma = {
			_stringList map{_.toUpperCase()}
			super.stringList
			  
			
	}
	
}

trait Randomize extends PreparaTimetable{
	abstract override def trasforma = {
	stringList = Random.shuffle(_stringList)
	 super.stringList
	
	}
}