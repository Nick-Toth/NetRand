{- ************************************************
\\ File:  NRTest.hs
// Name:  Nick G. Toth
\\ Email: ntoth@pdx.edu
// Date:  November 21, 2017
\\
// Overview: Testing / usage examples of the NetRand
\\ library. See README (especially), NetRand.hs and
// NRServer.hs for more information.
\\
// ************************************************ -}

import NetRand

main :: IO ()
main =

  -- Get the next 7 digits from the queue. Enqueue f 8675309.
  rn 8675309 >>= printSvNetRand

  -- Get the next 10 digits from the queue. Enqueue f 4815162342.
  -- rn_s "4815162342" >>= printSvrMsg

  -- Get the next 5 digits from the queue. Enqueue f 3141592654.
  -- rn_d 5 3141592654 >>= printSvNetRand

  -- Get the next 5 digits from the queue, without modifying it.
  -- peek 5 >>= printSvNetRand
