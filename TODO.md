  * Map vs. HashMap https://stackoverflow.com/questions/7894867/performant-haskell-hashed-structure
  * BUG (server): seems that some tiny instances are bugged, where they don't
    fill out bads correctly - in particular, the graph's head commit is set to
    be good, so when you filter the graph, it deletes everything
  * SO slow on raptor, dunno why - literally like 20x slower
    * thinking it's to do with the parallelism options
    * https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-concurrent.html#using-smp
    * https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-concurrent.html#rts-flag--N%20%E2%9F%A8x%E2%9F%A9
