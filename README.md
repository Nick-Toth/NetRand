# NetRand

An experiment in shared pseudo-random number generation.

NRServer should currently be running at 104.131.156.26:6969. If I shut it down, I will also update this README. i.e., If you're reading this and the server is down, please free to contact me so I can solve the problem. ENJOY!


## Overview Part I:

Last year, I thought it would be neat to create a pseudo-random number generator which leverages its users as a source of pseudo-random data. My original idea was simply to have clients request a string of random digits in exchange for a proportional numeric string of their own. The server's data would simply be a queue of data from previous users. For example, 
suppose Jenny wanted 7 random digits. In this case, Jenny would send something like "8675309". If the server's queue contained "4815162342.." at the time of Jenny's request, then she would receive "4815162", and the queue would become "342..8675309".


## Issues (and potential solutions):

Unfortunately, there are some major issues with the idea. These issues include, but are not limited to:

  - Server downtime could be a substantial issue for any applications which utilize this method as a random number source.
    I suspect this issue could be solved by creating a decentralized version of NetRand, but such an implementation could be       prone to other issues. In particular, a decentralized version would stipulate a considerable user-base. If this idea           catches on, however, I would love to give it a shot.

  - The queue is inherently finite. This could be problematic if Jenny wanted 10,000 random numbers, but the queue contained       only 5000. I haven't given much thought to solutions for this problem, although I'm confident it's not a prohibitive           issue.
  
  - SLOW. That's just reality. * On a related note, the fact that internet access is required might be problematic for some       projects requiring random numbers. *
  
  - Spam and carelessness could be a serious issue in practice. It's easy to imagine that clients might make requests that
    fill the queue with unreasonable sequences. Conversely, clients might go out of their way to avoid any sequences, which is     similarly problematic. I suspect that, with enough users, these issues would balance each other. In the case of this           program, I have implemented a solution which I will explain below.
    
  - Clients might receive their own previous input. The original idea was actually to use a stack. I suspect that using a         queue will solve this issue.


## Overview Part II:

After letting it marinate for a while, I finally decided to create this simplistic implementation of the idea. The biggest difference is that I have taken a substantial countermeasure to solve the issue of spam. Rather than directly enqueuing users' inputs, the server weaves ("012" ° "987" => "091827") their inputs with a hash of their IP address (no client records are kept). Hopefully, this will only need to be a temporary solution. Ultimately, I'd prefer to use the direct approach.


## Conclusion:

This is just an experiment. In its current form, NetRand is very unsafe and rather impure (in relation to the original idea).
My hope is that some people will find the idea interesting, and help develop it into a more pragmatic application. If you're interested in the idea, or if you have any comments, suggestions or concerns, PLEASE contact me at ntoth@pdx.edu. I'd love to hear your thoughts!!
