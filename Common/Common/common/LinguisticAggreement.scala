package common

import common.segment.{Segment, BaseSegment}

object LinguisticAggreement {
 def verbAgreement(clause : List[Segment], addSegment : Segment) : Boolean = {
   val segment = BaseSegment.createInfoSegment(addSegment)
   val addVerbs = segment.ActiveVerbs
   val addNom = segment.Subject
   
   val clauseInfo = clause.map(p => BaseSegment.createInfoSegment(p)) 
   val clauseVerbs = clauseInfo.map(p => p.ActiveVerbs).toList.flatten
   val clauseNom = clauseInfo.map( p => p.Subject).toList.flatten
   var aggreementInClause : Boolean = false
   var aggreementInSegment : Boolean = false
   if (!addVerbs.isEmpty  && !addNom.isEmpty)
   {
	   aggreementInSegment = this.compareVerbsAndNouns(addVerbs, addNom) 
   }
   if (!clauseVerbs.isEmpty && !clauseNom.isEmpty )
   {
	   aggreementInClause = this.compareVerbsAndNouns(clauseVerbs, clauseNom)
   }
   // pokud m�e b�t shoda p��sudku s podm�tem napln�na v aktu�ln�m objektu tak se nehled� mimo
   if (aggreementInClause || aggreementInSegment)
   {
	   false
   }
   if (!addVerbs.isEmpty && !clauseNom.isEmpty) 
   {
	   this.compareVerbsAndNouns(addVerbs, clauseNom)
   }
   else if (!clauseVerbs.isEmpty && !addNom.isEmpty) 
   {
	   this.compareVerbsAndNouns(clauseVerbs, addNom ::: clauseNom)
   }
   else 
   {
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
     if (verbPerson != '3' ) 
     {
       val pronomeSubject = subjects.filter(p => p.isPronome)
       if (pronomeSubject.isEmpty && subjects.length == 1)
       {
         false
       }
       else if (pronomeSubject.isEmpty)
       {
         true
       }
       else
       {
         pronomeSubject.filter(p => p.getPerson == verbPerson && p.getNumber == verbNumber).length > 0
       }
       
     }
     else {
       if (verbNumber == 'S' && subjects.filter(p => p.isSingular).length == 0 )
        {
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
       else if (subjects.filter(s => s.getPerson == verbPerson || (s.compareTag("N") && s.getNumber == verbNumber)).length == 0)
       {
         false
       }
       else 
       {
       true
       }
     }
   }
 }
  
}