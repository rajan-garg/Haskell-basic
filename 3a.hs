import Data.Char

findChar str
  | str >= 97 && str <= 122 = chr str             -- find character in that range
  | otherwise = findChar (str-36)             -- wrap around if out of range

rotation str map = findChar((ord str)+(ord map))      -- adding up the ASCII values of the two character.

encrypt str [] key = (encrypt str key key)          -- When the key run outs its characters, it again starts with its first character
encrypt [] map key =  []                  -- when whole text is encrypted
encrypt str map key                       -- str= text and map=key
  | ord (head str) >= 65 && ord(head str) <= 90 = (head str):(encrypt (tail str) map key)    -- uppercase letter, do not change that  | head str == '0' = '*':(encrypt (tail str) map key)
  | head str == '0' = '*':(encrypt (tail str) map key)           
  | head str == '1' = '`':(encrypt (tail str) map key)
  | head str == '2' = '~':(encrypt (tail str) map key)
  | head str == '3' = '!':(encrypt (tail str) map key)
  | head str == '4' = '@':(encrypt (tail str) map key)
  | head str == '5' = '#':(encrypt (tail str) map key)                 --all cases given in question
  | head str == '6' = '$':(encrypt (tail str) map key)
  | head str == '7' = '%':(encrypt (tail str) map key)
  | head str == '8' = '^':(encrypt (tail str) map key)
  | head str == '9' = '&':(encrypt (tail str) map key)
  | otherwise = (rotation(head str) (head map)):(encrypt (tail str) (tail map) key)    --adding up the ASCII values of the two character if not of above cases

main = do
  putStrLn "Enter the PlainText: "          -- string to be encrypted
  text <- getLine                            -- read string
  putStrLn "Enter the key: "                -- key used for encription
  key <- getLine                             -- read key
  putStr "The Ciphertext is: "
  putStrLn (encrypt text key key)                -- encrypted string
