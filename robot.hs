robot (n,a,h) = \message -> message (n,a,h)

getName robot = robot (\(n,a,h) -> n)
getAttack robot = robot (\(n,a,h) -> a)
getHP robot = robot (\(n,a,h) -> h)

setName robot newName = robot (\(n,a,h) -> (newName,a,h))
setAttack robot newAttack = robot (\(n,a,h) -> (n,newAttack,h))
setHealth robot newHealth = robot (\(n,a,h) -> (n,a,newHealth))

printRobot robot = ()

damage aRobot attackDamage = aRobot (\(n,a,h) -> (n,a,h-attackDamage))

fight aRobot defender = damage defender attack
  where attack = if getHP aRobot > 10
                 then getAttack aRobot
                 else 0

threeRoundFight robotA robotB = 
                               
                              
