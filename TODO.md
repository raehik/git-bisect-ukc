  * Map vs. HashMap https://stackoverflow.com/questions/7894867/performant-haskell-hashed-structure
  * BUG (server): seems that some tiny instances are bugged, where they don't
    fill out bads correctly - in particular, the graph's head commit is set to
    be good, so when you filter the graph, it deletes everything
  * SO slow on raptor, dunno why - literally like 20x slower
    * thinking it's to do with the parallelism options
    * https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-concurrent.html#using-smp
    * https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-concurrent.html#rts-flag--N%20%E2%9F%A8x%E2%9F%A9
  * benchmark my foldls and other things, see if I want `foldl'` and `seq`
    instead
    * christ I hate Haskell
    * https://wiki.haskell.org/Seq
    * https://stackoverflow.com/questions/26669871/why-seq-in-haskell-has-to-have-special-rule-for-bottom
    * https://stackoverflow.com/questions/12617664/a-simple-example-showing-that-io-doesnt-satisfy-the-monad-laws?noredirect=1&lq=1
    * also see par https://www.reddit.com/r/haskell/comments/6e6mu0/spark_pseq_seq_etc_need_help_understanding/
    * https://wiki.haskell.org/Par_and_seq
    * https://stackoverflow.com/questions/4576734/why-do-we-need-seq-or-pseq-with-par-in-haskell
  * cool check this out https://wiki.haskell.org/Difference_list
  * cool WOW https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html
