- dot concept
- pipeline concept
- {{.field.subfield}}
- {{- }} {{ -}}
- {{index | filter }}
- funcMap - https://golang.org/pkg/text/template/#example_Template_func
- wrapping like how std.fmt does it with custom formatters
- i'd build control flow on a stack pattern
  translate the template into IR and execute the IR
- range else
- unicode support

## DONE: 
- {{.field}} in range
- working scopes (detect duplicate keys)
- {{if .cond}} a {{else if .cond2}} b {{else}} c {{end}}
- {{- }} and {{ -}} to trim whitespace before / after an action
- initial pipes - only working for fields {{.field1 | .field2}}