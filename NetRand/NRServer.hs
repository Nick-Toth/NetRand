{- ************************************************
\\ File:  NRServer.hs
// Name:  Nick G. Toth
\\ Email: ntoth@pdx.edu
// Date:  November 19, 2017
\\
// Overview: Server side component to the NetRand
\\ app. Please see NetRand.hs for an overview of,
// the project as well as usage info. See README
\\ for a more detailed description of the peoject.
//
\\ ************************************************ -}

import Crypto.Hash

import System.IO
import System.Random
import System.IO.Unsafe

import Control.Monad (liftM2)
import Control.Concurrent.Thread as CT

import Network.Socket.Internal
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

import Data.List
import qualified Data.ByteString.Char8 as C


main :: IO ()
main = spinUp


{-
   The size of the queue. Note that upon
   starting the server, the queue will be
   initialized with queue_size numbers
   generated using System.Random. Once
   queue_size numbers have been exchanged/
   donated by various clients, the queue
   will be entirely made up of client inputs
   and hashed ip addresses.
-}
queue_size = 1000 :: Int


{- =============== Networking =============== -}


-- Server port number.
port = Just "6969" :: Maybe String


-- Initializes the server, starts reqHandler.
spinUp :: IO ()
spinUp = 
  withSocketsDo $ do

    -- Format socket specifications.
    addrinfos <- getAddrInfo
                (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                Nothing port

    let svraddr = head addrinfos -- Address of this server.

    -- Build the specified socket.
    sock <- socket (addrFamily svraddr) Stream defaultProtocol

    -- Start the server.
    bind sock (addrAddress svraddr)
    listen sock 1

    -- Start handling requests.
    reqHandler sock initQueue 0


{-
   After initializing the server, spinUp
   calls reqHandler, which handles all following
   server processes at the highest level. After
   establishing a connection and forking the
   client interface, reqHandler maintains the
   queue of random numbers.

   @param sock: The server socket.

   @param queue: The queue of random numbers.

   @param msg_id: Reqest counter.
-}
reqHandler :: Socket -> IO String -> Int -> IO ()
reqHandler sock queue msg_id = do

  -- Wait for, accept incoming requests.
  conn <- accept sock

  -- Handle connection in a dedicated thread.
  (nums_taken, gen_data) <- (CT.forkIO $ interface conn msg_id queue)
    >>= snd >>= result

  -- If the client's instruction is peek, or an error
  -- occurred, continue handling requests with same queue.
  if (read nums_taken) < 1 then reqHandler sock queue $! msg_id + 1
  -- Otherwise, update queue and continue handling requests.
  else do
        -- Remove returned numbers from the queue.
    let drop_queue = queue >>= return.drop (read nums_taken) :: IO String
        -- Generate new queue numers in proportion to those returned to the client.
        gen_queue = hashClient (read nums_taken) gen_data :: IO String
        -- Create a new queue from drop_queue and gen_queue.
        queue_u = (liftM2 (++)) drop_queue gen_queue
                    >>= (\x -> return $ take queue_size x)

    -- Uncomment to print out queue as it's updated.
    -- queue_u >>= (\x -> putStrLn $ "\nqueue: " ++ x ++ "\n")

    -- Continue handling requsts.
    reqHandler sock queue_u $! msg_id + 1


-- Forked request handler. Called from reqHandler.
interface :: (Socket, SockAddr) -> Int -> IO String -> IO (String, Client)
interface (sock, addr) msg_id queue = do

  -- Store incoming request.
  input' <- recv sock 4096

      -- Parse client's request.
  let client = parseRequest (show addr) (C.unpack input') msg_id
      inp = input client
      -- Send the first len elements of the queue.
      respond len = queue >>= (\x -> return $ take (min queue_size len) x) -- Build requested output.
                            >>= (\x -> return $ C.pack x) -- Convert output to ByteString.
                              >>= sendAll sock -- Send requested data to client.

  -- Determine, execute the client's request type.
  case take 1 $ instruction client of
    -- Remove, return the first n digits of the queue.
    "s" -> do respond inp_len; return (show inp_len, client); where inp_len = length inp
    -- Return the first n digits of the queue without modifying the queue.
    "p" -> do respond (read inp :: Int); return ("0", client);
    -- Return the number of digits specified by the number following d.
    "d" -> do respond taking; return (show taking, client);
              where taking = (read (drop 1 (instruction client)) :: Int)
    _ -> do respond (-1); return ("-1", client)



{- =============== Parsing / Formatting =============== -}


-- Valid Client instructions
client_instrs = [ "s", -- Standard.
                  "p", -- Peek.
                  "d"  -- Donation.
                ] :: [String]


-- Temporary storage for client information.
data Client = Client { input :: String,
                       address :: String,
                       instruction :: String,
                       msg_id :: Int
                     } deriving Show


-- Extracts the client's input and instruction.
parseRequest :: String -> String -> Int -> Client
parseRequest addr req msg_id = parseRequest' addr req "" msg_id

{-
   Recursive scanner for separating
   the client's input and instruction.
-}
parseRequest' :: String -> String -> String -> Int -> Client
parseRequest' addr (':':input) instr msg_id = Client input addr instr msg_id
parseRequest' addr (i:input) instr msg_id = parseRequest' addr input (instr ++ [i]) msg_id


-- Matrix to vector.
vec :: Monad m => [[a]] -> m [a]
vec strs = return $ vec' strs []
  where
    -- Recursively vectorize matrix.
    vec' [] fnl_str = fnl_str
    vec' (str:strs) fnl_str = vec' strs $ str ++ fnl_str


-- Remove '.'s from an ip address.
parseAddr :: String -> String
parseAddr ip = filter (/= '.') ip


-- Convert hex string to integer.
hexToDec :: String -> Integer
hexToDec hxStr
  | not (isHex hxStr) = -1 -- Invalid entry.
  | otherwise = hexWd $ reverse hxStr
    where hexWd [] = 0
          hexWd (h:ex) = (16 * (hexToDec ex) + hexCh h)
          -- Return value of an individual hex char.
          hexCh a = h a
            where h 'a' = 10; h 'b' = 11; h 'c' = 12;
                  h 'd' = 13; h 'e' = 14; h 'f' = 15;
                  h _ = read [a]


-- Returns true if a string contains only numbers 0-9.
isInteger :: String -> Bool
isInteger str = isInSet str "0123456789"


{-
   Returns true if a string contains only
   numbers 0-9 and letters a-f.
-}
isHex :: String -> Bool
isHex str = isInSet str "0123456789abcdef"

{-
   Returns true if a string contains
   only elements of some set. [] = false.
-}
isInSet :: Foldable t => String -> t Char -> Bool
isInSet str set = Data.List.foldl (&&) True bools && str /= ""
  where bools = (Data.List.map (\x -> Data.List.elem x set) str)



{- =============== Util =============== -}


{-
   Generate a string of pseudo random numbers
   for initializing the server's queue.
-}
initQueue :: IO String
initQueue = genRands
              >>= (\a -> return $ map show a)
                >>= vec


{-
   Generate a list of n pseudo-random numbers,
   by hashing client data.
-}
hashClient :: Int -> Client -> IO String
hashClient taken client
    -- If top is large enough to replace the taken numbers, return it.
  | length top >= taken = return top
    -- Generate numbers beyond top.
  | otherwise = (vec $ map (show . sha512 . C.pack)
                  (splitStr (min hlen (max 1 rem)) (drop hlen ip)))
                    >>= (\x -> return x)
  where
    -- Extract client data.
    (raw_input, ip) = (input client, address client)
    hlen = div (length ip) 2
    -- Initialize the top of the new queue by hashing
    -- half of the ip, and weave it with the raw input.
    top = weaveStrs raw_input $ show $ sha512 $ C.pack $ take hlen ip
    -- Calculate the number of numbers that need to be generated after top.
    rem = ceiling $ (fromIntegral (taken - (length top)) :: Float) / 128.0


{-
   Split a string into an array of
   strings of length n.
   e.g. p 2 "abcde" = ["ab","cd"]
-}
splitStr :: Int -> String -> [String]
splitStr n [] = []
splitStr n l = filter (\str -> length str == n) $ take n l : (splitStr n $ drop n l)


{- 
   Combine two strings s.t. (For example)
   "abc" "zyx" -> "xcybza".
   Used for combining the client's input and
   hashed address into a single string. Drops
   remaining chars if strings are different sizes.
-}
weaveStrs :: String -> String -> String
weaveStrs a b
  | length a < length b = weaveStrs' a b []
  | otherwise = weaveStrs' b a []
  where
    weaveStrs' [] b out = out
    weaveStrs' (a:as) (b:bs) out = weaveStrs' as bs out ++ [a] ++ [b]


{-
   512-bit sha hash algorithm for processing
   client data into random numbers.
-}
sha512 :: C.ByteString -> Integer
sha512 bs = hexToDec $ show (hash bs :: Digest SHA3_512)


{-
   Random number generator in
   constant applicative form.
-}
genRands :: IO [Int]
genRands = return $ take queue_size rands
  where
    range = (0, 9)
    gen = unsafePerformIO newStdGen
    rands = (randomRs range gen)


-- Validate client configuration.
validateClient :: Client -> Bool
validateClient client =
  if (isInteger $ input client)
  && (isInteger $ address client)
  && (elem instr_sym client_instrs)
    then True
  else False
  where instr_sym = take 1 $ instruction client


-- Formatted print for client info.
printClientMsg :: Client -> IO ()
printClientMsg client =
  putStrLn $ "Client {"
          ++ "\n  input:       " ++ input client 
          ++ "\n  ip address:  " ++ address client
          ++ "\n  instruction: " ++ instruction client
          ++ "\n  msg number:  " ++ show (msg_id client)
          ++ "\n}"
