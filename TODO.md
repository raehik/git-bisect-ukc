# Actual todos
  * Map vs. HashMap https://stackoverflow.com/questions/7894867/performant-haskell-hashed-structure
  * BUG (server): seems that some tiny instances are bugged, where they don't
    fill out bads correctly - in particular, the graph's head commit is set to
    be good, so when you filter the graph, it deletes everything

# Various notes
## Corrected ancestor filter/usage algorithm
There's an potential efficiency we've missed: we may not have to recalculate
ancestors every step. In particular, if we subgraph on a new bad commit, the
ancestors of every commit in that commit's subgraph stay unchanged. (I've tried
to determine if we can keep any ancestors when we subgraph on a new bad commit
list, but it looks like we can't without employing a long-winded algorithm - no
speedup.)

Now the commit graph looks like:

    Map GitCommit ([GitCommit], Set GitCommit, Bool)

### Subgraphing
Subgraphing function now takes another parameter `Bool ancestors_invalidated`.

   * If `True`, during the first phase, all commits are annotated noting that
     their ancestors must be updated before use.
   * Else, no annotations are changed.

This

---

### Subgraphing
  * remove every ancestor of the good commits
    * must recalculate ancestors if they're invalidated
    * simple foldl
  * take subgraph from bad
    * keep a Set of visited nodes

---

NEW new algorithm:

  * "Subgraph" ONE SIDE ONLY.
    * If we're replacing the bad, return the subgraph of the new bad.
      * Algo: Fastest graph traversal possible (depth or breath-first). Use
        `Map.member` of the subgraph.
      * No need to edit commit parents or ancestors, they cannot have changed.
    * If we're replacing the good, delete its ancestors (guaranteed calculated),
      then iterate through the graph and invalidate all commits' ancestors.

Subgraph:

  * Start with the new bad commit in the stack.
  * Look up the commit's parents in sg (fail if not present)
  * For each parent:
    * Check if we've already got it in sg
      * If yes, do nothing
      * If not, copy it over from g and add it to the stack (fail if not
        present)

Note that we will lookup most commits twice: once when copying, once when
finding its parents. But the finding parents lookup is on sg, so it's not a big
deal (on average faster than using g again).

---

  * Pass another value n when trying to find best bisect commit, maximum number
    of commits to calculate ancestors of before giving up
    * Needs depth-first search
    * Alternatively, pick at random
