package NOFun4ForseOK

import scala.util.Random

class PreparaTimetable (a:Aeroporto) {
  val aeroporto = a

  var _stringList = List[String]()
  def stringlist = _stringList
  
  def transforma(l:List[Aereo]) = {
    val (arrivi, partenze) = l partition (_.arrivo.equals(aeroporto))
    stringlist_(List.tabulate(arrivi.size)(i => "A") ++ List.tabulate(partenze.size)(i => "D"))
    
  }
  
  def stringlist_(l:List[String]) = {  
    _stringList = l
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