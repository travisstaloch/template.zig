 - {{item.field}}
 - {{if (cond) a else b}}
 - real scopes
 - {{index | filter }}
 - funcMap - https://golang.org/pkg/text/template/#example_Template_func
 - wrapping like how std.fmt does it with custom formatters
 - i'd build control flow on a stack pattern
   translate the template into IR and execute the IR