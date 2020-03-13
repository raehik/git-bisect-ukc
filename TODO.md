  * Map vs. HashMap https://stackoverflow.com/questions/7894867/performant-haskell-hashed-structure
  * BUG (server): seems that some tiny instances are bugged, where they don't
    fill out bads correctly - in particular, the graph's head commit is set to
    be good, so when you filter the graph, it deletes everything
