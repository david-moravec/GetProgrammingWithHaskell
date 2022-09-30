toPart recipient = "Dear " ++ recipient ++ ",\n"
bodyPart bookTitle = "this is " ++ bookTitle ++ ".\n"
endingPart author = "thanks,\n" ++ author

createEmail recipient bookTitle author = toPart recipient ++ bodyPart bookTitle ++ endingPart author

main = do
  print "Who is the email for?"
  recipient <- getLine
  print "What is the Title?"
  title <- getLine
  print "Who is the Author?"
  author <- getLine
  print (createEmail recipient title author)
