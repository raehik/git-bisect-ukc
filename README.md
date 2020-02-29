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

  * filter the graph to only the commits we care about
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

#### Graph filtering
Graph filtering is an application of the provided problem restrictions:

  * The first bad commit is in the current earliest known bad commit's subgraph.
  * The first bad commit is not in any of the known good commits' subgraphs.

It's pretty obvious when you think about it, and Radu straight up placed it in
clear (though mathematical) terms in the assignment brief.

So, at every step of the algorithm, we make sure that we only query commits
which match both of these statements.
