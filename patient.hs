
data Name = Name { firstName :: String
                 , middleName :: String
                 , lastName :: String}
showName :: Name -> String
showName name = firstName name ++ " " ++ middleName name ++ " " ++ lastName name

data Sex = Female | Male
showSex :: Sex -> String
showSex Female = "F"
showSex Male = "M"

data RhType = Pos | Neg
showRH :: RhType -> String
showRH Pos = "+"
showRH Neg = "-"

data ABOType = A | B | AB | O
showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

data BloodType = BloodType ABOType RhType
showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRH rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False --otherwise

canPatientDonateTo :: Patient -> Patient -> Bool
canPatientDonateTo patient1 patient2 = canDonateTo blood1 blood2
  where blood1 = bloodType patient1
        blood2 = bloodType patient2
    


data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType}


printName, printSex, printAge, printHeight, printWeight, printBloodType :: Patient -> String
printName patient = showName (name patient) ++ "\n"
printSex patient = showSex (sex patient) ++ "\n"
printAge patient = show (age patient) ++ "\n"
printHeight patient = show (height patient) ++ "\n"
printWeight patient = show (weight patient) ++ "\n"
printBloodType patient = showBloodType (bloodType patient) ++ "\n"

patientSummary :: Patient -> String
patientSummary patient = stars ++ printName patient ++ 
                         printSex patient ++ 
                         printAge patient ++ 
                         printHeight patient ++
                         printWeight patient ++ 
                         printBloodType patient ++ stars
  where stars = "************\n"

jackieSmith = Patient {name = Name "Jackie" " " "Smith"
                      , age = 43
                      , sex = Female
                      , height = 62
                      , weight = 115
                      , bloodType = BloodType AB Neg }

peteDavid = Patient {name = Name "pete" "peter" "david"
                    , age = 3
                    , sex = Male
                    , height = 88
                    , weight = 115
                    , bloodType = BloodType A Pos }