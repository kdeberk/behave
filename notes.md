
Decision: Domain types
Domain types represent an enumerated subset of a literal type, e.g. [1, 2, 3] or ["foo", "bar"].
  The type association needs to be made explicit, because the DSL
  tries to do as little type inference as possible.
  
  Syntax: `type <name> domain <type> [<value1>, <value2, ...]
 
  Thought: perhaps we should only allow domains to be defined in the
  stimuli and struct label definitions, as I think they're only useful
  there; the domains are used to narrow the values that the solver should choose from.
