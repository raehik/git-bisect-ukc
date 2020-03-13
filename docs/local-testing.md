# Local testing
I whipped up a YAML parser and reimplemented the client logic to simply check
commit statuses from the parsed data. Super easy to use, and as type-safe as you
can care about considering we're already in IO.

    GitBisect.Parser.processRepoFile "examples/simple-filter.yaml"

## Overview
*Check the `examples` directory for exact syntax. It's the server JSON cleaned
up.*

A repo file holds a list of repos. Repos have a name, a graph, and a list of
instances. Instances each have a name, one initial known good+bad commit, and a
list of the bad commits. (The good commits are thus the ones not in that list.)
The only conversion we do for the data types is the turning the bad commits list
to a Set.

`processRepoFile` gives you back either a `Left error` (on a file reading or
parsing error) or a `Right` containing a list of answers for each repo. Every
answer is either a `Left Error` indicating some issue during solving,
or a `Right Solution`. A solution is the winning commit, and the number of
questions it took to get there.

With all this aforementioned error handling, we're well set to write test suites
and check how they fare.

## Notable findings
### 2020-03-13
It appears some of the tiny instances may have an error which violates the spec.
This whole thing was prompted by Joe Ling having strange issues with some tests:
I wanted to test it on my algorithm too. So I did -- see
`examples/joe-problematic.yaml`. Turns out, yeah, I'm pretty some instances are
invalid due to how the bads were picked - they're missing one very important
commit from the bads, the head. For me, the error manifested as "missing commit
[head commit]". Weird, because you never remove the head commit, only subgraph
filter it away. It happened because the head commit was found good, so it got
deleted. Along with... the rest of the graph.
