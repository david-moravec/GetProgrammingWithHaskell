type class Die

data FiveSidedDie = F1 | F2 | F3

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6

instance Show SixSidedDie where
  show S1 = show 1
  show S2 = show 2 
  show S3 = show 3
  show S4 = show 4
  show S5 = show 5
  show S6 = show 6

instance Eq SixSidedDie where
  (==) S6 S6 = True
  (==) S5 S5 = True
  (==) S4 S4 = True
  (==) S3 S3 = True
  (==) S2 S2 = True
  (==) S1 S1 = True
  (==) _ _ = False

data Name = Name (String, String) deriving (Show, Eq)

instance Ord Name where
  compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

names :: [Name]
names = [Name ("David", "Moravec"),
         Name ("Cyril", "Metodej"),
         Name ("ahoj", "cus")]
