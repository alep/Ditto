Ditto
=====
A pure Erlang IRC client
------------------------

I started this project to learn Erlang. It started as a IRC robot, but turned into a client the moment I wanted to add a GUI and learn wxErlang.

When I started with the robot, the idea was to hide the IRC protocol using one of the Erlang behaviours. I found that the most suitable was the gen_event behaviour, mostly because of the eventful nature of the IRC protocol. 

Although I did try using gen_fsm and gen_server, but it seemed, that these behaviours didn't fit, at least from my point of view.

The idea of hiding the IRC protocol behing a gen_event behaviour remains. Although lately I'm thinking it might not be a good idea or it's wasteful.

The following (ascii)-diagram shows, what's yet to be implemented, how would I hide using gen_event an error msg from the server when trying to subscribe a nick.

::
  +----------+                +----------+        gen_event:         +----------+
  |          |   NICK anick   |          |  notify({nick, "anick"})  | wxErlang |
  |IRC Server|  <-----------  |gen_event |  <----------------------  |   GUI    |
  |          |                |          |                           |          |
  +----------+                +----------+                           +----------+

                                    |  
                                    | State = "sent nick"
                                    |
                                   \/

  +----------+                   +----------+                       +----------+
  |          | ERR_NICKNAMEINUSE |          |                       | wxErlang |
  |IRC Server|   --------------> |gen_event |                       |   GUI    |
  |          |                   |          |                       |          |
  +----------+                   +----------+                       +----------+
                                    
                                    |
                                    | State = "sent pass" 
                                    | (or whatever before nick)
                                    \/

                                               (need some sort of 
  +----------+                   +----------+   callback into the GUI)  +----------+
  |          |                   |          |   UIPid ! { badnick }     | wxErlang |
  |IRC Server|                   |gen_event |   --------------------->  |   GUI    |
  |          |                   |          |                           |          |
  +----------+                   +----------+                           +----------+
                                    
I think it is wasteful because I don't think I need gen_event to hide all this, I could
just use the process that controls the socket which talks to gen_event. It might make
things simpler. 

Anyway I'm looking for comments on the idea and the code.

Thanks.
