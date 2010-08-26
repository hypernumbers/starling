
Starling is a Unicode text processing library for Erlang.
It's based on ICU (http://icu-project.org/), which you need to have installed to use Starling.

`port install icu` with MacPorts on OSX
`apt-get install libicu38 libicu-dev` on Ubuntu

You also need to have Ruby and Rake installed (the build script uses Rake).

To compile, just run `rake` from Starling's root directory.

Once you've compiled Starling, start the Erlang shell with:
`erl -pa ~/path/to/starling/ebin`
  
You can then use Starling like this:
  
  1> application:start(starling_app).
  ok
  2> S = ustring:new("hello").
  <<0,104,0,101,0,108,0,108,0,111>>
  3> S2 = ustring:upcase(S).
  <<0,72,0,69,0,76,0,76,0,79>>
  4> ustring:pr(S2).
  "HELLO"

There's a couple of simple demos under demos/. You may also want to take a look at tests/.

Project's homepage: http://12monkeys.co.uk/starling
Google Code page:   http://code.google.com/p/starling/

Hasan Veldstra <hasan@12monkeys.co.uk>