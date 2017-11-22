{- ************************************************
\\ File:  NetRand.hs
// Name:  Nick G. Toth
\\ Email: ntoth@pdx.edu
// Date:  November 19, 2017
\\
// Overview: The client and interface for a communal
\\ pseudo-random number generator. In essence, this
// program works by exchanging numbers between its
\\ users. This project is an experiment, more than
// anything else. The client program contains several
\\ functions for exchanging random numbers with the
// NetRand server. Generally speaking, though, the input
\\ to these functions is YOUR made up random number.
// The server will return to you a string of numbers
\\ in proportion to your input. Please see README.md
// for a more exhaustive description of this project.
\\ See RTest.hs for usage examples.
//
\\ ************************************************ -}


module NetRand (
  rn, rn_i, -- Exchange an Integer for an Integer.
            -- See function header comment and README!!
  rn_s, -- Exchange a string for a string.
  rn_d, -- See header comment.
  peek, -- View the top n digits from the queue. (String)
  printSvrMsg, printSvrBs, printSvNetRand, printSvrErr
) where

import Control.Exception

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C


{- =============== Random Numbers =============== -}


{-
   The most simple random number request function.
   rn returns an integer with as many digits as the
   one given. rn 09345, for example, would return a
   five digit number. Subsequently, the server would
   push 09345 onto the queue for some other person.
-}
rn :: Integer -> IO Integer
rn input = do
  svr_msg <- requestRN input $ Just "s"
  return $ bstoi svr_msg
rn_i input = rn input -- Alias


{-
   The only difference between this function and rn
   is that this function takes a string, rather than
   an integer.
-}
rn_s :: String -> IO String
rn_s input =  do
  svr_msg <- requestRN (read input :: Integer) $ Just "s"
  return $ C.unpack svr_msg


{-
   Like rn and rn_s, this function returns some specific
   number of random digits. Unlike those functions, however,
   this function handles the exchange in a slightly different
   way. When using this function, req_len should be the number
   of digits you want to recieve. Donation is a string of any
   number of random digits to be added to the server's queue.
   Please note that making a donation with fewer digits will
   mean that your HASHED ip address will make a/greater
   contribution to the queue. I DO NOT KEEP IP LOGS.. Think of
   it like scavenging. There is no immediate advantage to you
   in using this function. There is, however, an advantage in
   that you will be increasing the randomness of the generator.
   i.e., Please DO use this function.
-}
rn_d :: Integer -> Integer -> IO Integer
rn_d req_len donation = do
     svr_msg <- requestRN donation $ Just $ "d" ++ show req_len
     return $ bstoi svr_msg


{-
   Returns the first n digits from the NetRand queue, but doesn't
   remove them. Remember when peeking that it's very possible
   that the queue could change before going back! This function
   could be useful, for example, if you only want one random
   number per some large unit of time.
-}
peek :: Integer -> IO Integer
peek count = do
  svr_msg <- requestRN count $ Just "p"
  return $ bstoi svr_msg



{- =============== Networking =============== -}


{-
   Server information. This is not a placeholder. This
   is the info for my server, which (should be) currently
   running this program. Feel free to try it out! Also
   feel free to change the info and make your own server!
-}
(server_ip, port) = (Just "104.131.156.26", Just "6969")


{-
   This function currently serves as the sole point
   of contact to the NetRand server.

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
        -- Extract symbol from msg
        instr_sym = msg >>= (\arr -> return $ take 1 arr)

    -- Prep socket for contacting server.
    sock <- socket (addrFamily svr_addr) Stream defaultProtocol
    -- Establish connection to server.
    connect sock (addrAddress svr_addr)

    -- Identify and send request to server.
    case instr_sym of
      Just "s" -> sendAll sock $ packMsg input_num "s"
      -- Treated the same as Just "s".
      Nothing -> sendAll sock $ packMsg input_num "s"
      Just "p" -> sendAll sock $ packMsg input_num "p"
      -- Donation. (Formatted differently - dinput_num:donation)
      Just "d" -> mapM_ (\x -> sendAll sock $ packMsg input_num x) msg

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


{-
   Converts a bytestring into a number.
   If the string starts with 0, move the
   0 to the end, ... until not 0. This is
   done so that users recieve the requested
   number of digits.
-}
bstoi :: C.ByteString -> Integer
bstoi msg = bstoi' msg_s (length msg_s)
  where
    msg_s = C.unpack msg
    bstoi' msg cnt
      | cnt == 0 = 0
      | otherwise =
          if head msg_s == '0'
            then bstoi' (msg ++ "0") $ cnt-1
          else read msg_s

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
printSvNetRand :: Integer -> IO ()
printSvNetRand num = printSvrMsg $ show num

-- Print an error message.
printSvrErr :: String -> String -> IO ()
printSvrErr err_code err_msg =
  putStrLn $ "\n  Server Error #"
          ++ err_code ++ ": "
          ++ err_msg ++ "\n"
