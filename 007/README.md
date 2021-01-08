Day 7 looks like a search problem where the search space is defined by expansions of one bag into multiple bags based on color type.  For example:

`light red bags contain 1 bright white bag, 2 muted yellow bags.`

I think this will lend itself to the cons structure like so

```
(defparameter *rules*
	      ("light red bag" . '( ( "bright white bag" . 1 ) ( "muted yellow bag" . 2 ) ) )
	      ("dark orange bag" . '( ( "bright white bag" . 3 ) ( "muted yellow bag " . 4 ) ) )
	      ("bright white bag" . '( ( "shiny gold bag" . 1 ) ) ) )
```

Search can be performed by being given an end state and performing the search with each rule as a starting node.

Search works by taking the CAR of a rule, checking to see if it matches the goal, and if not, calling search recursively on the CDR of the rule

