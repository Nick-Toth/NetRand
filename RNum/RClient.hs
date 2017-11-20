{- ************************************************
\\ File:  RClient.hs
// Name:  Nick G. Toth
\\ Email: ntoth@pdx.edu
// Date:  November 19, 2017
\\
// Overview: 
\\
// ************************************************ -}


module RNum (
  rn, rn_i, -- Exchange an Integer for an Integer.
  rn_s, -- Exchange a string for a string.
  peek -- View the top n digits from the stack. (String)
) where


import Control.Exception

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C


test :: Integer -> IO ()
test num = do
  rand <- rn num
  printSvrNum rand
  return ()


{- =============== Random Numbers =============== -}

{- The most simple random number request function.
   rn returns an integer with as many digits as the
   one given. rn 09345, for example, would return a
   five digit number. Please see header comment for
   usage instructions. -}
rn :: Integer -> IO Integer
rn input = do
  svr_msg <- requestRN input $ Just "std"
  return $ bstoi svr_msg
rn_i input = rn input -- Alias


{- The only difference between this function and rn
   is that this function takes a string, rather than
   an integer. -}
rn_s :: String -> IO String
rn_s input =  do
  svr_msg <- requestRN (read input :: Integer) $ Just "std"
  return $ C.unpack svr_msg


{- Returns the first n digits from the rnum stack, but doesn't
   remove them. Remember when peeking that it's very possible
   that the stack could change before going back!
   This function could be useful, for example, if you only want
   one random number per some large unit of time.  -}
peek :: Integer -> IO Integer
peek count = do
  svr_msg <- requestRN count $ Just "peek"
  return $ bstoi svr_msg


{- =============== Networking =============== -}

-- Server information.
(server_ip, port) = (Just "104.131.156.26", Just "6969")


{- This function currently serves as the sole point
   of contact to the RNum server.

   @param input_num: The number being traded with the server.

   @param msg: Specific instructions for the server
               in processing the request.

   @return: The servers response.
-}
requestRN :: Integer -> Maybe String -> IO C.ByteString
requestRN input_num msg =
  withSocketsDo $ do
    -- Format server details.
    svr_info <- getAddrInfo Nothing server_ip port
    let svr_addr = head svr_info

    -- Prep socket for contacting server.
    sock <- socket (addrFamily svr_addr) Stream defaultProtocol
    -- Establish connection to server.
    connect sock (addrAddress svr_addr)

    -- Identify and send request to server.
    case msg of
      Just "std"  -> sendAll sock $ packMsg input_num "std"
      -- Treated the same as Just "std".
      Nothing     -> sendAll sock $ packMsg input_num "std"
      Just "peek" -> sendAll sock $ packMsg input_num "peek"

    -- Store the server's response.
    msg_i <- recv sock 4096
  
    -- Terminate the connection.
    close sock
    -- Return the server's response.
    return msg_i



{- =============== Formatting =============== -}

-- Converts a number into a bytestring.
itobs :: Integer -> C.ByteString
itobs msg = C.pack $ show msg

-- Converts a bytestring into a number.
bstoi :: C.ByteString -> Integer
bstoi msg = read $ C.unpack msg

-- Prepare a message and numeric input to send to the server.
packMsg :: Integer -> String -> C.ByteString
packMsg num msg = C.pack $ msg ++ ":" ++ show num



{- =============== Local IO =============== -}

-- Print a string with a "server: " label.
printSvrMsg :: String -> IO ()
printSvrMsg msg = putStrLn $ "\n  Server: " ++ msg ++ "\n"

-- Print a bytestring with a "server: " label.
printSvrBs :: C.ByteString -> IO ()
printSvrBs msg_bs = printSvrMsg $ C.unpack msg_bs

-- Print a number with a "server: " label.
printSvrNum :: Integer -> IO ()
printSvrNum num = printSvrMsg $ show num

-- Print an error message.
printSvrErr :: String -> String -> IO ()
printSvrErr err_code err_msg =
  putStrLn $ "\n  Server Error #"
          ++ err_code ++ ": "
          ++ err_msg ++ "\n"
