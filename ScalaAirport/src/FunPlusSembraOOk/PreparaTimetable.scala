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


class PreparaTimetable (a:Aeroporto/*, l:List[Aereo]*/) {
  val aeroporto = a

  /* var _stringList: List[String] = recInit(l)//init//
 
    def stringList = _stringList
 
    def stringList_=(stringList: List[String]) = _stringList = stringList*/
    
     
  
  def init(l:List[Aereo]) = {
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
    rest match {
       case Nil => Nil
      case head :: tail => generateTimetable(a, (head.partenza, head.arrivo) ) :: recInit(tail)
     
    }
  }
  
  def trasforma(l:List[String]) = {
    //println("in super")
    l}
  
  
}

trait Normalizza extends PreparaTimetable{
  
  

	override def trasforma(l:List[String]) = {
			val ln = l map{_.toUpperCase()}
			super.trasforma(ln)
			  
			
	}
	
}

trait Randomize extends PreparaTimetable{
	override def trasforma(l:List[String]) = {
	val lr = Random.shuffle(l)
	//println("randomizzo");
	 super.trasforma(lr)
	
	}
}
	
	trait StartWithDeparture extends PreparaTimetable{
	  def allArrivals(rest:List[String]) : Boolean = rest match {
	    case head::Nil => head match {
	      case "A" => true
	      case "D" => false
	    }
	    case Nil => false
	    case head :: tail => head match {
	      case "A" => allArrivals(tail)
	      case "D" => false
	    }
	    
	  }
	  
	 
	 
	  
	  override def trasforma(l:List[String]) = {//println("con D all'inizio")
	    allArrivals(l) match {
	      case true => super.trasforma(l)
	      case false if l.size > 1 => (l(0), l(1)) match {
	        case ("D", x:String) => super.trasforma(l)
	       
	         
	        case ("A", "D") => val l1 = "D" :: "A" :: l.drop(2)
	       // println(l1)
	         super.trasforma(l1)
	        case ("A", "A") => val firstP = l.indexOf("D")
	        val g1 = l.drop(1) 
	        val tail = (l).drop(firstP + 1)
	        val middle1 = (g1 diff tail)
	        val middle = middle1.dropRight(1)
	        val l2 = ("D" :: middle) ::: "D" :: tail
	        //println(l2)
	         super.trasforma(l2)
	        
	        			
	        					}
	      	case _ => super.trasforma(l)		
	    }
	    
	   
	  }
	}
