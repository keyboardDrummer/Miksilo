EditorParser
=======
A parsing library that creates parsers suitable for use in text editors. Parsers are specified using a [parser combinator](https://en.wikipedia.org/wiki/Parser_combinator) API, which allows defining parsers in a style similar to writing [BNF](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form) grammars but with more flexibility. EditorParser allows specifying (indirect) [left recursive parsers](https://en.wikipedia.org/wiki/Left_recursion), indentation sensitive parsers required for languages like YAML and Python, and custom context-sensitive parsers. EditorParsers's default choice operator is unordered, so consumers don't have to worry about the order.

Parsers used for text editors have unique requirements when compared to parsers used for machine consumption, which EditorParser satisfies:
- Parsers must be error correcting, meaning that when the parser encounters errors in the input, it can correct them and continue parsing. This feature is important because while a user is typing there are often parse errors in the input text, and without error correction it's not possible to provide good feedback to the user. Most importantly, without error correction features like code completion won't work at the times when they're most useful!
- Parsers must be incremental, meaning that if the user makes a small change in the input text, only this piece of text needs to be re-parsed. This is important to allow the editor tooling to remain responsive even when large files are edited.

### Error Correction
In general, an error correction parser can produce many different results for a given input text, since adding an removing input symbols can be done in many different ways. To determine which of the parse results is most likely to be what the human writer intended to write, heuristics are used to score each parse result, after which the best scoring one is picked. 

Determining and scoring all possible error corrected parses is often too time consuming. EditorParsers will pause evaluating a parse when it encounters an error, and then it assigns a temporary score to that partial parse. The temporary score determines how likely the partial parse is to have a high score when fully parsed, and is used to prioritize the order in which partial parses are evaluated. Since the score of a partial parse can continuously change while it's being further evaluated, EditorParser can switch between evaluating different partial parses if they all maintain a similar score.

EditorParsers uses a stop criteria passed by the caller to determine how much time to spend on evaluating parse results. Examples of stop criteria are:
 - stop parsing as soon as possible, which would be after the first parse is found
 - evaluate all possible parses
 - parse for at most X amount of time

### Left Recursion
Top-down parsers generally have trouble supporting left-recursion. One solution is to detect left recursion when it occurs. At this point, we have on the call stack the initial call to the (indirect) recursive parser, and the recursive call to it. We don't know yet what to return for the recursive call. Given this state, we can imagine a function that takes the result of the recursive call and uses that to return the result of the initial call. If we find the [fixpoint](https://en.wikipedia.org/wiki/Fixed_point_(mathematics)) of this function, then we have the result of the initial call.  

The paper [Packrat Parsers Can Support Left Recursion](http://www.vpri.org/pdf/tr2007002_packrat.pdf) finds this fixpoint by repeatedly evaluating the initial parser. In the first evaluation, the recursive call will return an empty result; in the second evaluation the recursive call will return the result of the first run, and so forth until the parse result does not change any more by returning it in the recursive call of a new evaluation. However, this approach runs into trouble because of the memoization that is used to get the right performance characteristics. The memoization prevents the repeated evaluations of the same parser from returning different results, and the paper spends tremendous effort in correcting this problem. In the paper [Direct Left-Recursive Parsing Expression Grammars](https://tratt.net/laurie/research/pubs/html/tratt__direct_left_recursive_parsing_expression_grammars/), Laurence Tratt notes that "Warth et al. first present a relatively simple adaption of Packrat parsing to cope with direct left-recursion, before detailing a much more complicated adaption to cope with indirect left-recursion". An extensive technical report on an implementation of this algorithm is presenting in [Packrat Parsing in Scala](http://www.scala-archive.org/attachment/1956909/0/packrat_parsers.pdf).

EditorParser uses a novel approach to finding the fixpoint mentioned above by treating it more purely. In EditorParser, if a left recursive call is detected, the returned parse result is not a value, but a function that takes a parse result and returns a parse result, which initially is simply the identity function. This function is propagated up the call stack, during which it can be modified by other parsers, until it reaches the initial call of the left recursive parser, at which point a loop is run to find the fixpoint of this function. There is no conflict with memoization, since all the parsers are evaluated only once for a given input position. No extra measures have to be taken to also handle indirect left-recursion.

As mentioned above, EditorParser parsers can not only return a value but also a function as a parse result. To get a unified interface for both types of results, we use a monadic interface. This interface is used for example by the sequence operator, whose implementation conceptually looks like: 

```
for {
  left <- parseleft(input)
  right <- parseRight(left.remainder)
} yield {
  val resultValue = combine(left.value, right.value)
  ValueResult(resultValue, right.remainder)
}
```

Note that manipulating parse results through a monadic interface introduces a performance overhead, since more function calls are required than when manipulating values directly.

### Unordered choice and associativity
_Work in progress_

EditorParsers has an unordered choice operator, which allows parsing valid input where greedy choice operators would not. Consider the example where input `abc` is parsed with grammar `(ab | a) bc`. If a greedy choice operator is used, the `ab` grammar will be picked over `a` in the first choice, causing the `bc` grammar to fail. This example is contrived, but supporting these cases means the grammar writing has to consider one less category of problems.

In contrast to EditorParser, the popular category of Packrat parsers ([paper](https://bford.info/pub/lang/packrat-icfp02.pdf)) has a greedy choice operator so it will fail on cases like the one presented above.

### Indentation sensitive parsing
_Work in progress_

### Performance
_Work in progress_
