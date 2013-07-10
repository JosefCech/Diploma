package common

import common.segment.{Segment, BaseSegment}

object LingvisticAggreement {
 def verbAgreement(clause : List[Segment], addSegment : Segment) : Boolean = {
   val segment = BaseSegment.createInfoSegment(addSegment)
   val addVerbs = segment.ActiveVerbs
   val addNom = segment.Subject
   
   val clauseInfo = clause.map(p => BaseSegment.createInfoSegment(p))
   val clauseVerbs = clauseInfo.map(p => p.ActiveVerbs).toList.flatten
   val clauseNom = clauseInfo.map( p => p.Subject).toList.flatten
  
   if (!addNom.isEmpty && addNom.head.form == "obèanství")
   {
      println(segment.segment)
   println("slovesa")
   println(clauseVerbs.map(p => p.toString).mkString(" "))
   println("nom")
   println(addNom.map(p => p.toString).mkString(" "))
   
   println("----------------------------------------")
   println("slovesa")
   println(addVerbs.map(p => p.toString).mkString(" "))
   println("nom")
   println(clauseNom.map(p => p.toString).mkString(" "))
    println("----------------End------------------------")
   }
  
   if (!addVerbs.isEmpty && !clauseNom.isEmpty) {
   /*   println(segment.segment)
   println("slovesa")
   println(addVerbs.map(p => p.toString).mkString(" "))
   println("nom")
   println(clauseNom.map(p => p.toString).mkString(" "))*/
   this.compareVerbsAndNouns(addVerbs, clauseNom)
   }
   else if (!clauseVerbs.isEmpty && !addNom.isEmpty) {
      
     this.compareVerbsAndNouns(clauseVerbs, addNom ::: clauseNom)
   }
   else {
       true
   }
 }
 
 def compareVerbsAndNouns(verbs : List[MorfWord], subjects : List[MorfWord]) : Boolean = {
   val selVerbs = verbs.filter(p => p.tag.charAt(7) != 'X')
   val pastForm = verbs.filter(p => p.compareTag("Vp"))
   val presentForm = verbs.filter(p => p.compareTag("VB"))
   if (selVerbs.isEmpty) {
     true
   } else {
     val selVerb = selVerbs.head 
     val verbPerson = {
       if (presentForm.isEmpty)
       {
         selVerb.getPerson
       }
       else {
         presentForm.head.getPerson
       }
     }
      
     val verbNumber = {
       if (presentForm.isEmpty)
       {
         selVerb.getNumber
       }
       else {
         presentForm.head.getNumber
       }
     }
      
     val verbGender = {
       if (pastForm.isEmpty)
       {
         selVerb.getGender
       }
       else {
         pastForm.head.getGender
       }
     }
      
     val countSubjects = subjects.length
     println( verbGender.toString + verbNumber.toString + verbPerson.toString) 
     if (verbPerson == '1' ) {
       println(subjects)
       println(subjects.filter(p => p.isPronome).length)
       subjects.filter(p => p.isPronome).length > 0
     }
     else if (verbPerson != '3')
     {
      true  
     }
     else {
       if (verbNumber == 'S' && subjects.filter(p => p.isSingular).length == 0 ) {
          println(subjects.filter(p => p.isSingular).length)
         false
       } 
       else if (verbNumber == 'P' && countSubjects == 1 && subjects.head.isSingular )
       {
         false
       }
       else if (verbNumber == 'P' && countSubjects == 1 && (subjects.head.getGender != verbGender && verbGender != '-'))
       {
         false
       }
       else {
         println("true")
       true
       }
     }
   }
 }
  
}