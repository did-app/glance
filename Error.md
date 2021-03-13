


https://jsonapi.org/format/#errors
id -> context -> tracing_id
code -> 
title -> summary, grouping key
detail -> human readable version

pgo
http
request

In typed language only

Acl
CastFailure
NotProvided


Header 'foo' was not present
<!-- Fails validation -->
Header 'foo' is not a valid email address

Have module my_app/service/db
Have module my_app/service/http_client

In sql db connection allowed failure invalid syntax developer error




```rust
service/pgo

try = pgo.run(sql, args, mapper)
// ,leaves single error type

// vs, second approach allows handling of failed casting.
try rows = pgo.run(sql, args)
assert users = list.map(rows, row_to_user)
assert [user] = users

```

```rust
import perimeter/http_request/headers
import perimeter/keyed_list is headers

result.all6(
  required(request, 'Foo', as_string)
  optional()
  User()
)

// Doesn't work as ok(none) looses error info
result.all6(
  header.from(request, 'Foo', as_int) |> required()
  header.from(request, 'Bar', as_int) |> fallback(3)
  from(request, Header("X-Foo"), as_int)
)
```


<!-- neither 400 or 422 get reported in the main application -->
```rust
perimeter.Report{
  Report(
    code -> title
    detail
  )
}

Report(PhantomLibrary){

}

perimeter.group(List(Report(Perimeter)))


pgo.to_report(conn)(
  case error<QueryError> {
    SyntaxError -> Report(InputError, "Invalid SQL", "Could not process query")
    ConnectionUnavailable -> Report(ServiceUnavailable, "DB unavailable")
    // ConnectionDenied ->
    MissingTable -> Report()
    MissingColum
    ConstraintError -> Report(Unprocessable, "Constraint Error")
  }
)

// Meta map(String, String) Or Json

try conn = db.connect(config)
try server = listen(port)
list.each(server.connections(server), handle)
```