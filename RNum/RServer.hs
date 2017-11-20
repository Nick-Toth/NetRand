
{- ************************************************
\\ File:  RServer.hs
// Name:  Nick G. Toth
\\ Email: ntoth@pdx.edu
// Date:  November 19, 2017
\\
// Overview: This is pretty incomplete, but hopefully
\\ putting it on github tonight will motivate me to
// finish the general idea tomorrow morning.
\\
// ************************************************ -}

import System.IO
import System.Random

import Crypto.Hash

import Control.Concurrent.Thread as CT
import Control.Monad (liftM2)

import Network.Socket.Internal
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

import Data.List
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = spinUp


{- =============== Networking =============== -}


-- Server port number.
port = Just "6969" :: Maybe String


{- Initializes up the server, and starts reqHandler. -}
spinUp :: IO ()
spinUp = withSocketsDo $ do

  -- Format socket specifications.
  addrinfos <- getAddrInfo
              (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
               Nothing port
  let svraddr = head addrinfos

  -- Build the specified socket.
  sock <- socket (addrFamily svraddr) Stream defaultProtocol

  -- Start the server.
  bind sock (addrAddress svraddr)
  listen sock 1

  -- Start handling requests.
  reqHandler sock stack_init 0


{- After initializing the server, spinUp
   calls reqHandler, which handles all following
   server processes at the highest level. After
   establishing a connection and forking the
   client interface, reqHandler maintains the
   stack of random numbers.
   @param sock: The server socket.
   @param stack: The stack of random numbers.
   @param msg_num: Reqest counter.
-}
reqHandler :: Socket -> IO String -> Int -> IO ()
reqHandler sock stack msg_num = do

  -- Wait for, accept incoming requests.
  conn <- accept sock

  a <- stack
  putStrLn $ "\nstack: " ++ a ++ "\n"

  -- Handle connection in a dedicated thread.
  (nums_taken, gen_data) <- (CT.forkIO $ interface conn msg_num stack)
    >>= snd >>= result

  -- If the client's instruction is peek, or an error
  -- occurred, continue handling requests with same stack.
  if nums_taken < 1 then reqHandler sock stack $! msg_num + 1
  -- Otherwise, update stack and continue handling requests.
  else do
        -- Remove returned numbers from the stack.
    let drop_stack = stack >>= return.drop nums_taken :: IO String
        -- Generate new stack numers in proportion to those returned to the client.
        gen_stack = hashClient nums_taken gen_data :: IO String
        -- Create a new stack from drop_stack and gen_stack.
        stack_u = (liftM2 (++)) drop_stack gen_stack

    -- Continue handling requsts.
    reqHandler sock stack_u $! msg_num + 1


{- Forked request handler. Called from reqHandler. -}
interface :: (Socket, SockAddr) -> Int -> IO String -> IO (Int, Client)
interface (sock, addr) msg_num stack = do

  -- Store incoming request.
  input' <- recv sock 4096

      -- Parse client's request.
  let client = parseRequest (show addr) (C.unpack input') msg_num
      inp = input client
      inp_len = length inp
      -- Send the first len elements of the stack.
      respond len = stack >>= (\x -> return $ take len x) -- Build requested output.
                            >>= (\x -> return $ C.pack x) -- Convert output to ByteString.
                              >>= sendAll sock -- Send requested data to client.
      -- Exit this function, s.t. rem numbers will be removed from the stack.
      exit rem = return (rem, client) -- do close sock; return (rem, client);

  putStrLn $ "Instr :: " ++ instruction client

  -- Determine, execute the client's request type.
  case instruction client of
    -- Remove, return the first n digits of the stack.
    "std" -> do respond inp_len; exit inp_len;
    -- Return the first n digits of the stack without modifying the stack.
    "peek" -> do respond (read inp :: Int); exit 0;

  -- Client's request is incomplete or invalid. Return failure message.
  sendAll sock $ C.pack $ "Invalid Instruction: " ++ instruction client

  -- Exit with error code.
  return (-1, client)



{- =============== Parsing / Formatting =============== -}


-- Temporary storage for client information.
data Client = Client { input :: String,
                       address :: String,
                       instruction :: String,
                       msg_num :: Int
                     } deriving Show


-- Extracts the client's input and instruction.
parseRequest :: String -> String -> Int -> Client
parseRequest addr req msg_num = parseRequest' addr req "" msg_num


{- Recursive scanner for separating
   the client's input and instruction. -}
parseRequest' :: String -> String -> String -> Int -> Client
parseRequest' addr (':':input) instr msg_num = Client input addr instr msg_num
parseRequest' addr (i:input) instr msg_num = parseRequest' addr input (instr ++ [i]) msg_num


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


{- Convert hex string to integer. Unsafe unless
   input is guaranteed to be composed of valid
   hex chars. i.e., ['0'..'9']++['a'..'f'] -}
parseHex :: String -> Integer
parseHex hxStr = hexWd (reverse hxStr)
    where hexWd []     = 0
          hexWd (h:ex) = hexCh h + 16 * parseHex ex -- Do you see what I did there ;)
          -- Return value of an individual hex char.
          hexCh a = h a
            where h 'a' = 10; h 'b' = 11; h 'c' = 12;
                  h 'd' = 13; h 'e' = 14; h 'f' = 15;
                  h _ = read [a]


{- =============== Server IO =============== -}


-- Formatted print for client info.
printClientMsg :: Client -> IO ()
printClientMsg client =
  putStrLn $ "Client {"
          ++ "\n  input:       " ++ input client 
          ++ "\n  ip address:  " ++ address client
          ++ "\n  instruction: " ++ instruction client
          ++ "\n  msg number:  " ++ show (msg_num client)
          ++ "\n}"


{- =============== Util =============== -}


-- Maximum size of the stack.
max_stack = 2500 :: Int

{- The size of the initial stack,
   generated with System.Random. -}
init_size = 100 :: Int

{- Generate a string of pseudo random numbers
   for initializing the server's stack. -}
stack_init = hashClient init_size $ Client "init" "0" "std" 0 :: IO String


{- Generate a list of n pseudo-random
   numbers, by hashing client data. -}
hashClient :: Int -> Client -> IO String
hashClient nums client = (hashClient' nums) >>= vec
  where hashClient' n = (prList n) >>= (\a -> return $ map show a)


-- 512-bit sha hash algorithm for processing client data.
sha512 :: C.ByteString -> Integer
sha512 bs = parseHex $ show (hash bs :: Digest SHA3_512)


-- Generate a list of n pseudo-random numbers.
prList :: Int -> IO [Int]
prList 0 = return []
prList n = do
  r  <- randomRIO (0, 9)
  rs <- prList (n-1)
  return (r:rs)