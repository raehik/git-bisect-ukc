# CO663 PLAD Git bisect assignment
  > ["I've been to hell and back, and back to hell…and back."][lion-quote] Lion, Dota 2

[lion-quote: https://gamepedia.cursecdn.com/dota2_gamepedia/1/12/Vo_lion_lion_respawn_01.mp3

I spent what probably amounts to *days* poring over this problem with others and
by myself. Lots of fun, sometimes infuriating. I resolved to use Haskell, which
I now know means I enjoy inflicting pain on myself.

I found out some cool things about the problem and Haskell. This README is just
for presenting all that - no need to actually read it.

## Algorithm
tl;dr At every step:

  * filter irrelevant commits (ones that can't be the first bad commit)
  * select a bisect commit by calculating commit ranks until we
    1. find an ideal bisect commit,
    2. get to the end of the graph, or
    3. run out of allowed calculations (in which case we use the best commit so
       far)

When the graph is size 1, you have your first bad commit.

Now let's get into the gorey details.

### Terminology
  * **Ancestors:** Full commit history. Includes the commit itself.
  * **Subgraph:** The graph of the of ancestors of a certain commit.

### Overview
The algorithm can be split into two steps:

  1. Filter the graph into only the relevant commits.
  2. Select the best bisect commit we can find.

All decent solutions should implement something that can be classified as such.
There are simple algorithms that solve each of these steps independently of the
other relatively quickly. Upon inspection, however, there ends up being lots of
recalculation, which could be crippling for huge graphs. Let's take the chance
to make it as efficient as possible!

### Filtering the graph
Graph filtering is an application of the provided problem restrictions:

  * The first bad commit is in the current earliest known bad commit's subgraph.
  * The first bad commit is not in any of the known good commits' subgraphs.

It's pretty obvious when you think about it, and Radu straight up placed it in
clear (though mathematical) terms in the assignment brief.

So, at every step of the algorithm, we make sure that we only query commits
which match both of these statements.

### Selecting a bisect commit
[git-bisect-docs]: https://git-scm.com/docs/git-bisect-lk2009

This part is all to do with efficient ranking and unwasteful graph traversal. I
will assume familiarity with the [Git bisect doc page][git-bisect-docs] we were
given.

Our aim is to select a commit that halves the graph, regardless of the query
result. If we can do that every time, we essentially achieve a binary search,
and a repository of size n takes at most `log2 n` questions to solve. This is
our goal.

As in the Git bisect docs, we can rank commits as follows:

    f(X) = min(no_of_ancestors(X), graph_size - no_of_ancestors(X))

So providing we can calculate every commit's ancestors, we can easily select the
commit with the highest rank. However, calculating ancestors is painful for
large graphs: it costs a lot of memory to calculate a list of ancestors for a
repository with 1,000,000 commits. I haven't done the maths, but I'm going to
assume that number is approximately *too big no thanks*.

Instead, we'll compromise: calculate the ancestors of the first X commits you
can, and return the best commit we found along the way. This way, even if the
graph is huge, we can do our best to calculate a few commits that may be useful
to bisect on. And on smaller graphs, X will be larger than the total graph size,
so we'll calculate and check every commit. *(Note that depth-first traversal
important here, as we want to find commits at the "middle" of the graph.)*

#### Considerations
  * Worse on linear graphs than highly branched ones.
    * Consider the shape of those graphs and our traversal. In a fully linear
      graph, we have to traverse 50% of the commits and calculate *all* the
      ancestors along the way. In a mega branched graph, we may find 5 middling
      commits from 5 short branches almost immediately.
    * This is a sad fact of this algorithm :(

### Sneaky speedups
#### No need to check subgraphs under a certain size
Say you have a graph of size 11. The whole graph has its ancestors calculated
(the previously queried commit was bad). You are partways through the bisect
commit selection step, and you find a commit with size 3.

It doesn't beat your current best commit, and it's not the best possible commit.
You're about to add its parents to your stack to check them next - but hold on.
You're below the halfway point, so smaller subgraphs aren't going to be useful.
And subgraphs exclusively hold commits with fewer ancestors.

When we find a commit with size `ceiling(graph_size/2)`, we can disregard its
parents, because we know that they won't be a better bisect commit than the
current one. (Obviously this check comes after the is-better-than-current-best
check.)

#### Moving the earliest known bad commit does not invalidate ancestors
TODO. Pretty nice, means ~50% of the time the next step gets to be a simple
partial DFS traversal (partial because of the above point on subgraph sizes)

### Other thoughts
A cheap, dumb, stupid algorithm could be to DFS until your seen set is
`ceiling(graph_size/2)`, at which point you select the commit you're sitting on
now. In theory, in a linear-ish graph (especially one where non-merging branches
are short), you end up around the halfway point. That'd beat my selection
algorithm the F out for repos which are mostly just the master branch or have
few root commits. But mine's still worthwhile for finding the perfect commit
efficiently, when you have the time and memory for it.

An even dumber algorithm is to select a random commit every time. A friend found
out that this worked unexpectedly well and I'm very cross about it. Really the
only thing that matters is to filter every step...
