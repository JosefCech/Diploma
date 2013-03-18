package Rules.Interface

import common.{ Segment , TaggedSegment }

trait IAutomata {
 def step( state: Int,
           q : List[(Int , (Int,String), Int)],  // prechodova funkce 
           f : List[(Int,Int => Int )], // uzaviraci funkce
           segment : Segment,
           previousLevel: Int
         ) : (Int,Boolean,Segment) = {
   val tagSegment = (new TaggedSegment(segment)).GetTag
   val rules = q.filter(p => p._1 == state && satisfiedRule(p._2,tagSegment) )
   if (rules.size > 0) {
     val rule = rules.head
     val nextState = rule._3
     val functions = f.filter(p => p._1 == state)
     if (functions.isEmpty)
     {
       // jdi na dalsi stav
      (nextState,false,null) 
     }
     else {
     val func = functions.head._2
     segment.SetLevel(func(previousLevel))
      // jdi na dalsi stav a zapis ziskana data - vetsinou se vraci
     (nextState,true,segment)
     } 
   }
   else if (state != 0) {
     (0,false,null)
   }
   else {
     throw new Exception("Unknown path")
   }
   
 }
 
 private def satisfiedRule(condition : (Int,String),tag : String) : Boolean = {
   tag.substring(condition._1, condition._1 + condition._2.size).toString == condition._2
 }
 
 private def addLevel(p : Int) : Int = p+1
 
 private def subtractLevel(p : Int) : Int = List( p-1, 0 ).max
 
 private def sameLevel(p :Int) : Int = p
}