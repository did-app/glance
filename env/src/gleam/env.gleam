
// escript gleam_providers "gleam/env" "filename"
// generate "escript fof"
// call already running
// This is useful as is.
// Can generate from .spec.json
// Do mix deps.compile
// Have a providers compiler
// Load up all modules called provider
// Find extension from provider
// Read all files and pass them to the generate module
// gleam_providers as a package
// compile deps
// When running the mix task, it should have all deps
// called gleam@provider@something
// Mix gleam.provide json_schema "foo.com"
// Help with the rust, hash in to map of already defined
// but return no error in that case.

// Mix compilers -> escripts -> extension

// provide gleam/env
// Do the hard thing and call external BUT I have to do rust and a syntax

// Take the module
// search for a function and pass the arguments
// make it into a thing you blat down on it's own
// better than external OS command
// gleam/provider/csv.derive("foo.text") (foo.com)
// How to add dependency to escript, just looks like a function call
