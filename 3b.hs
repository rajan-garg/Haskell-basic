import Data.Char

findChar str
  | str >= 97 && str <= 122 = chr str             -- find character in that range
  | otherwise = findChar (str+36)             -- wadd 26 around if out of range

rotation str map = findChar((ord str)-(ord map))      -- subtracting up the ASCII values of the two character.

decrypt str [] key = (decrypt str key key)          -- When the key run outs its characters, it again starts with its first character
decrypt [] map key =  []                  -- when whole text is decrypted
decrypt str map key                       -- str= text and map=key
  | ord (head str) >= 65 && ord(head str) <= 90 = (head str):(decrypt (tail str) map key)    -- uppercase letter, do not change that  | head str == '0' = '*':(decrypt (tail str) map key)
  | head str == '*' = '0':(decrypt (tail str) map key)           
  | head str == '`' = '1':(decrypt (tail str) map key)
  | head str == '~' = '2':(decrypt (tail str) map key)
  | head str == '!' = '3':(decrypt (tail str) map key)
  | head str == '@' = '4':(decrypt (tail str) map key)
  | head str == '#' = '5':(decrypt (tail str) map key)                 --all cases given in question
  | head str == '$' = '6':(decrypt (tail str) map key)
  | head str == '%' = '7':(decrypt (tail str) map key)
  | head str == '^' = '8':(decrypt (tail str) map key)
  | head str == '&' = '9':(decrypt (tail str) map key)
  | otherwise = (rotation(head str) (head map)):(decrypt (tail str) (tail map) key)    --adding up the ASCII values of the two character if not of above cases

main = do
  putStrLn "Enter the Ciphertext: "          -- string to be decrypted
  text <- getLine                            -- read string
  putStrLn "Enter the key: "                -- key used for encryption
  key <- getLine                             -- read key
  putStr "The Plaintext is: "
  putStrLn (decrypt text key key)                -- decrypted string