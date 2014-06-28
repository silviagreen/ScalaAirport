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


class PreparaTimetable (a:Aeroporto) {
  val aeroporto = a

  var _stringList = List[String]()
  def stringlist = _stringList
  def stringlist_= (l:List[String]) = _stringList = l
  
  
  def transforma(l:List[Aereo]) = {
   // val (arrivi, partenze) = l partition (_.arrivo.equals(aeroporto))
    val generateTimetable = new createTimetable
    var i = -1
    val timetable = 1 to l.size map{ _=> i = i + 1 
        								generateTimetable(a, (l(i).partenza, l(i).arrivo) ) }
    
    stringlist = timetable.toList
   // stringlist_(List.tabulate(arrivi.size)(i => "A") ++ List.tabulate(partenze.size)(i => "D"))
    
  }
  
  
  
}

trait Normalizza extends PreparaTimetable{
  
  

	abstract override def stringlist = {
			_stringList map{_.toUpperCase()}
			super.stringlist
			  
			
	}
	
}

trait Randomize extends PreparaTimetable{
	abstract override def stringlist = {
	_stringList = Random.shuffle(_stringList)
	 super.stringlist
	
	}
}