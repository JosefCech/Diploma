package Translation

object Translation {

def succesTotal : String = "Celková úspěšnost"
def succesComplex : String =  "úspěšnost v souvětí"
def succesComplexWith(n : Any) : String = "úspěšnost v souvětí" + n.toString + "větami"
def succesSplitSentence : String = "úspěšnost ve větách s rozděleněmi klauzemi"
def wrongTagged : String = "wrong tagged segment"

  

}