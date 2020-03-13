# Network
I was relatively new to writing Haskell when I started this assignment. One of
the more fun things I learned about was writing imperative-style code in a
fail-friendly manner. Writing the network parts of the assignment allowed me to
(made me) do lots of that.

## Overview
The client communicates with the server by sending and receiving messages. These
are encoded/decoded between JSON and data types @ `GitBisect.Network.Messages`.
Much of the boilerplate is handled by the JSON library I use.

The client stays in the `ExceptT Error IO a` monad at all times, which
essentially lets me keep an `Either Error a` inside another monad (the IO
monad). This gives me a way to write clean imperative-style code that can fail,
without having to nest a bazillion if-else clauses. At the same time, it does
mean that much of the code ends up being "wrap type a in type b" and so on. But
isn't that what good Haskell is all about?

The client is modelled as a state machine, where each state is a function.
There's one annoying state where we don't know what type the next message might
be, so we have to try one and then the other.
