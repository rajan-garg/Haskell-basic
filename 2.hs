import Text.Numeral.Grammar( defaultInflection )
import qualified Text.Numeral.Language.ENG as ENG

food=[(4719,"FISH FINGERS",121,10),(1234,"CHICKEN LOLLY",250,10),(1111,"PANEER PAKODA",170,10)]         -- database of food (code, name, pric, quantity)
 
findBill order bill = do                    -- recursive function to take order
  putStrLn "Enter code:"                    -- get code of food
  foodCode <- readLn                        -- read code
  let eat = filter (\ (a,_,_,_)->a==foodCode) food             -- get food items from database with code= foodCode
  if 1==length eat                           -- if there is 1 food item
    then do putStrLn "Enter Quantity:"
            q <- readLn                      -- read quantity needed
            let quantity = (\ (_,_,_,a)->a)(eat!!0)        --quantity available
            if quantity > q                                -- if available
              then do putStr "AVAILABLE"
                      let money = (\ (_,_,a,_)->a)(eat!!0)     -- get price of food
                      let totalBill = q*money                 -- total bill for that item
                      let updateOrder = order ++ [show foodCode ++ "     "++ (\ (_,a,_,_)->a)(eat!!0) ++ "     "++ show money ++ "     "++ show q ++ "     "++ show totalBill++"\n"]
                      let grandBill= bill + totalBill      -- grand bill for all items
                      putStrLn "More Items? (Y/N):"
                      input <- getLine
                      if input=="Y"
                        then do findBill updateOrder grandBill

                        else do return (updateOrder, grandBill)

              else do putStr "NOT AVAILABLE"               -- if not available
                      putStrLn "More Items? (Y/N):"
                      input <- getLine
                      if input=="Y"
                        then do findBill order bill

                        else do return (order,bill) 

    else do putStrLn "**WRONG CODE, NO ITEM FOUND**"        -- if wrong code for food
            putStrLn "More Items? (Y/N):"
            input <- getLine
            if input=="Y"
                        then do findBill order bill

                        else do return (order,bill)  

main = do
  (order, bill) <- findBill [] 0                -- order is list of items in bill , bill is amount
  putStrLn "ALCHERINGA 2018, STALL 14: TANGO FAST FOOD CENTER"
  putStr $ concat order
  putStrLn "--------------------------------------------------"
  putStrLn ("                                          " ++ show bill)
  print (ENG.gb_cardinal defaultInflection $ bill)

