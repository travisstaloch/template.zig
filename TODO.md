 - dot concept
 - pipeline concept
 - {{.field.subfield}}
 - {{if cond}} a {{end}}
 - {{- }} {{ -}}
 - {{index | filter }}
 - funcMap - https://golang.org/pkg/text/template/#example_Template_func
 - wrapping like how std.fmt does it with custom formatters
 - i'd build control flow on a stack pattern
   translate the template into IR and execute the IR
 - range else

## DONE: 
- {{.field}} in range
- working scopes (detect duplicate keys)