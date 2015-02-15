# polyglot-readme

Lots of programming language snippets together in a single README. Good for testing syntax highlighting themes.


## bash

```bash
#!/bin/bash

###### BEGIN CONFIG
ACCEPTED_HOSTS="/root/.hag_accepted.conf"
BE_VERBOSE=false
###### END CONFIG

if [ "$UID" -ne 0 ]
then
 echo "Superuser rights is required"
 echo 'Printing the # sign'
 exit 2
fi

if test $# -eq 0
then
elif test [ $1 == 'start' ]
else
fi

genApacheConf(){
 if [[ "$2" = "www" ]]
 then
  full_domain=$1
 else
  full_domain=$2.$1
 fi
 host_root="${APACHE_HOME_DIR}$1/$2/$(title)"
 echo -e "# Host $1/$2 :"
}

```

## clojure

```clojure
; Comment

(def
  ^{:macro true
    :added "1.0"}
  let (fn* let [&form &env & decl] (cons 'let* decl)))

(def ^:dynamic chunk-size 17)

(defn next-chunk [rdr]
  (let [buf (char-array chunk-size)
        s (.read rdr buf)]
  (when (pos? s)
    (java.nio.CharBuffer/wrap buf 0 s))))

(defn chunk-seq [rdr]
  (when-let [chunk (next-chunk rdr)]
    (cons chunk (lazy-seq (chunk-seq rdr)))))

```

## coffeescript

```coffeescript
grade = (student, period=(if b? then 7 else 6), messages={"A": "Excellent"}) ->
  if student.excellentWork
    "A+"
  else if student.okayStuff
    if student.triedHard then "B" else "B-"
  else
    "C"

square = (x) -> x * x

two = -> 2

math =
  root:   Math.sqrt
  square: square
  cube:   (x) -> x * square x

race = (winner, runners...) ->
  print winner, runners

class Animal extends Being
  constructor: (@name) ->

  move: (meters) ->
    alert @name + " moved #{meters}m."

hi = `function() {
  return [document.title, "Hello JavaScript"].join(": ");
}`

heredoc = """
CoffeeScript subst test #{ 010 + 0xf / 0b10 + "nested string #{ /\n/ }"}
"""

###
CoffeeScript Compiler v1.2.0
Released under the MIT License
###

OPERATOR = /// ^ (
?: [-=]>             # function
) ///

```

## cpp

```cpp
#include <iostream>
#define IABS(x) ((x) < 0 ? -(x) : (x))

int main(int argc, char *argv[]) {

  /* An annoying "Hello World" example */
  for (auto i = 0; i < 0xFFFF; i++)
    cout << "Hello, World!" << endl;

  char c = '\n';
  unordered_map <string, vector<string> > m;
  m["key"] = "\\\\"; // this is an error

  return -2e3 + 12l;
}

```

## css

```css
@media screen and (-webkit-min-device-pixel-ratio: 0) {
  body:first-of-type pre::after {
    content: 'highlight: ' attr(class);
  }
  body {
    background: linear-gradient(45deg, blue, red);
  }
}

@import url('print.css');
@page:right {
 margin: 1cm 2cm 1.3cm 4cm;
}

@font-face {
  font-family: Chunkfive; src: url('Chunkfive.otf');
}

div.text,
#content,
li[lang=ru] {
  font: Tahoma, Chunkfive, sans-serif;
  background: url('hatch.png') /* wtf? */;  color: #F0F0F0 !important;
  width: 100%;
}

```

## dart

```dart
library app;
import 'dart:html';

part 'app2.dart';

/**
 * Class description and [link](http://dartlang.org/).
 */
@Awesome('it works!')
class SomeClass<S extends Iterable> extends BaseClass<S> implements Comparable {
  factory SomeClass(num param);
  SomeClass._internal(int q) : super() {
    assert(q != 1);
    double z = 0.0;
  }

  /// **Sum** function
  int sum(int a, int b) => a + b;

  ElementList els() => querySelectorAll('.dart');
}

String str = ' (${'parameter' + 'zxc'})';
String str = " (${true ? 2 + 2 / 2 : null})";
String str = r'\nraw\';
String str = r"\nraw\";
var str = '''
Something ${2+3}
''';
var str = r"""
Something ${2+3}
""";

```

## glsl

```glsl
// vertex shader
#version 150
in  vec2 in_Position;
in  vec3 in_Color;

out vec3 ex_Color;
void main(void) {
    gl_Position = vec4(in_Position.x, in_Position.y, 0.0, 1.0);
    ex_Color = in_Color;
}


// geometry shader
#version 150

layout(triangles) in;
layout(triangle_strip, max_vertices = 3) out;

void main() {
  for(int i = 0; i < gl_in.length(); i++) {
    gl_Position = gl_in[i].gl_Position;
    EmitVertex();
  }
  EndPrimitive();
}


// fragment shader
#version 150
precision highp float;

in  vec3 ex_Color;
out vec4 gl_FragColor;

void main(void) {
    gl_FragColor = vec4(ex_Color, 1.0);
}

```

## erlang

```erlang
-module(ssh_cli).

-behaviour(ssh_channel).

-include("ssh.hrl").
%% backwards compatibility
-export([listen/1, listen/2, listen/3, listen/4, stop/1]).

if L =/= [] ->      % If L is not empty
    sum(L) / count(L);
true ->
    error
end.

%% state
-record(state, {
    cm,
    channel
   }).

-spec foo(integer()) -> integer().
foo(X) -> 1 + X.

test(Foo)->Foo.

init([Shell, Exec]) ->
    {ok, #state{shell = Shell, exec = Exec}};
init([Shell]) ->
    false = not true,
    io:format("Hello, \"~p!~n", [atom_to_list('World')]),
    {ok, #state{shell = Shell}}.

concat([Single]) -> Single;
concat(RList) ->
    EpsilonFree = lists:filter(
        fun (Element) ->
            case Element of
                epsilon -> false;
                _ -> true
            end
        end,
        RList),
    case EpsilonFree of
        [Single] -> Single;
        Other -> {concat, Other}
    end.

union_dot_union({union, _}=U1, {union, _}=U2) ->
    union(lists:flatten(
        lists:map(
            fun (X1) ->
                lists:map(
                    fun (X2) ->
                        concat([X1, X2])
                    end,
                    union_to_list(U2)
                )
            end,
            union_to_list(U1)
        ))).

```

## go

```go
package main

import (
    "fmt"
    "os"
)

const (
    Sunday = iota
    numberOfDays  // this constant is not exported
)

type Foo interface {
    FooFunc(int, float32) (complex128, []int)
}

type Bar struct {
    os.File /* multi-line
               comment */
    PublicData chan int
}

func main() {
    ch := make(chan int)
    ch <- 1
    x, ok := <- ch
    ok = true
    float_var := 1.0e10
    defer fmt.Println('\'')
    defer fmt.Println(`exitting now\`)
    var fv1 float64 = 0.75
    go println(len("hello world!"))
    return
}

```

## handlebars

```handlebars
<h3>Hours</h3>

<ul>
  {{#each content.users}}
  <li {{bindAttr hello="world"}}>{{firstName}}</li>
  {{/each}}
</ul>

```

## haskell

```haskell
{-# LANGUAGE TypeSynonymInstances #-}
module Network.UDP
( DataPacket(..)
, openBoundUDPPort
, openListeningUDPPort
, pingUDPPort
, sendUDPPacketTo
, recvUDPPacket
, recvUDPPacketFrom
) where

import qualified Data.ByteString as Strict (ByteString, concat, singleton)
import qualified Data.ByteString.Lazy as Lazy (ByteString, toChunks, fromChunks)
import Data.ByteString.Char8 (pack, unpack)
import Network.Socket hiding (sendTo, recv, recvFrom)
import Network.Socket.ByteString (sendTo, recv, recvFrom)

-- Type class for converting StringLike types to and from strict ByteStrings
class DataPacket a where
  toStrictBS :: a -> Strict.ByteString
  fromStrictBS :: Strict.ByteString -> a

instance DataPacket Strict.ByteString where
  toStrictBS = id
  {-# INLINE toStrictBS #-}
  fromStrictBS = id
  {-# INLINE fromStrictBS #-}

openBoundUDPPort :: String -> Int -> IO Socket
openBoundUDPPort uri port = do
  s <- getUDPSocket
  bindAddr <- inet_addr uri
  let a = SockAddrInet (toEnum port) bindAddr
  bindSocket s a
  return s

pingUDPPort :: Socket -> SockAddr -> IO ()
pingUDPPort s a = sendTo s (Strict.singleton 0) a >> return ()

```

## haxe

```haxe

// quicksort example from http://haxe.org/doc/snip/quicksort
class Quicksort {

    static var arr = [4,8,0,3,9,1,5,2,6,7];

    static function quicksort( lo : Int, hi : Int ) : Void {
        var i = lo;
        var j = hi;
        var buf = arr;
        var p = buf[(lo+hi)>>1];
        while( i <= j ) {
            while( arr[i] > p ) i++;
            while( arr[j] < p ) j--;
            if( i <= j ) {
                var t = buf[i];
                buf[i++] = buf[j];
                buf[j--] = t;
            }
        }
        if( lo < j ) quicksort( lo, j );
        if( i < hi ) quicksort( i, hi );
    }

    static function main() {
        quicksort( 0, arr.length-1 );
        trace(arr);
    }
}

```

## http

```http
POST /task?id=1 HTTP/1.1
Host: example.org
Content-Type: application/json; charset=utf-8
Content-Length: 19

{"status": "ok", "extended": true}

```

## ini

```ini
;Settings relating to the location and loading of the database
[Database]
ProfileDir=.
ShowProfileMgr=smart
Profile1_Name[] = "\|/_-=MegaDestoyer=-_\|/"
DefaultProfile=True
AutoCreate = no

[AutoExec]
use-prompt="prompt"
Glob=autoexec_*.ini
AskAboutIgnoredPlugins=0

```

## json

```json
[
  {
    "title": "apples",
    "count": [12000, 20000],
    "description": {"text": "...", "sensitive": false}
  },
  {
    "title": "oranges",
    "count": [17500, null],
    "description": {"text": "...", "sensitive": false}
  }
]

```

## javascript

```javascript
function $initHighlight(block, flags) {
  try {
    if (block.className.search(/\bno\-highlight\b/) != -1)
      return processBlock(block.function, true, 0x0F) + ' class=""';
  } catch (e) {
    /* handle exception */
    var e4x =
        <div>Example
            <p>1234</p></div>;
  }
  for (var i = 0 / 2; i < classes.length; i++) { // "0 / 2" should not be parsed as regexp
    if (checkCondition(classes[i]) === undefined)
      return /\d+[\s/]/g;
  }
  console.log(Array.every(classes, Boolean));
}
```

## less

```less
/*
Using the most minimal language subset to ensure we
have enough relevance hints for proper Less detection
*/

@import "fruits";

@rhythm: 1.5em;

@media screen and (min-resolution: 2dppx) {
    body {font-size: 125%}
}

section > .foo + #bar:hover [href*="less"] {
    margin:     @rhythm 0 0 @rhythm;
    padding:    calc(5% + 20px);
    background: #f00ba7 url(http://placehold.alpha-centauri/42.png) no-repeat;
    background-image: linear-gradient(-135deg, wheat, fuchsia) !important ;
    background-blend-mode: multiply;
}

@font-face {
    font-family: /* ? */ 'Omega';
    src: url('../fonts/omega-webfont.woff?v=2.0.2');
}

.icon-baz::before {
    display:     inline-block;
    font-family: "Omega", Alpha, sans-serif;
    content:     "\f085";
    color:       rgba(98, 76 /* or 54 */, 231, .75);
}

```

## lisp

```lisp
#!/usr/bin/env csi

(defun prompt-for-cd ()
   "Prompts
    for CD"
   (prompt-read "Title" 1.53 1 2/4 1.7 1.7e0 2.9E-4 +42 -7 #b001 #b001/100 #o777 #O777 #xabc55 #c(0 -5.6))
   (prompt-read "Artist" &rest)
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
  (if x (format t "yes") (format t "no" nil) ;and here comment
  )
  ;; second line comment
  '(+ 1 2)
  (defvar *lines*)                ; list of all lines
  (position-if-not #'sys::whitespacep line :start beg))
  (quote (privet 1 2 3))
  '(hello world)
  (* 5 7)
  (1 2 34 5)
  (:use "aaaa")
  (let ((x 10) (y 20))
    (print (+ x y))
  )
```

## lua

```lua
--[[
Simple signal/slot implementation
]]
local signal_mt = {
    __index = {
        register = table.insert
    }
}
function signal_mt.__index:emit(... --[[ Comment in params ]])
    for _, slot in ipairs(self) do
        slot(self, ...)
    end
end
local function create_signal()
    return setmetatable({}, signal_mt)
end

-- Signal test
local signal = create_signal()
signal:register(function(signal, ...)
    print(...)
end)
signal:emit('Answer to Life, the Universe, and Everything:', 42)

--[==[ [=[ [[
Nested ]]
multi-line ]=]
comment ]==]
[==[ Nested
[=[ multi-line
[[ string
]] ]=] ]==]

```

## makefile

```makefile
# Makefile

BUILDDIR      = _build
EXTRAS       ?= $(BUILDDIR)/extras

.PHONY: main clean

main:
	@echo "Building main facility..."
	build_main $(BUILDDIR)

clean:
	rm -rf $(BUILDDIR)/*

```

## markdown

```markdown
# hello world

you can write text [with links](http://example.com) inline or [link references][1].

* one _thing_ has *em*phasis
* two __things__ are **bold**

[1]: http://example.com

---

hello world
===========

<this_is inline="xml"></this_is>

> markdown is so cool

    so are code segments

1. one thing (yeah!)
2. two thing `i can write code`, and `more` wipee!


```

## nginx

```nginx
user  www www;
worker_processes  2;
pid /var/run/nginx.pid;
error_log  /var/log/nginx.error_log  debug | info | notice | warn | error | crit;

events {
    connections   2000;
    use kqueue | rtsig | epoll | /dev/poll | select | poll;
}

http {
    log_format main      '$remote_addr - $remote_user [$time_local] '
                         '"$request" $status $bytes_sent '
                         '"$http_referer" "$http_user_agent" '
                         '"$gzip_ratio"';

    send_timeout 3m;
    client_header_buffer_size 1k;

    gzip on;
    gzip_min_length 1100;

    #lingering_time 30;

    server {
        server_name   one.example.com  www.one.example.com;
        access_log   /var/log/nginx.access_log  main;

        rewrite (.*) /index.php?page=$1 break;

        location / {
            proxy_pass         http://127.0.0.1/;
            proxy_redirect     off;
            proxy_set_header   Host             $host;
            proxy_set_header   X-Real-IP        $remote_addr;
            charset            koi8-r;
        }

        location /api/ {
            fastcgi_pass 127.0.0.1:9000;
        }

        location ~* \.(jpg|jpeg|gif)$ {
            root         /spool/www;
        }
    }
}

```

## python

```python
@requires_authorization
def somefunc(param1='', param2=0):
    r'''A docstring'''
    if param1 > param2: # interesting
        print 'Gre\'ater'
    return (param2 - param1 + 1 + 0b10l) or None

class SomeClass:
    pass

>>> message = '''interpreter
... prompt'''

```

## rust

```rust
use std;

#![warn(unstable)]

/* Factorial */
fn fac(n: int) -> int {
    let s: str = "This is
a multi-line string.

It ends with an unescaped '\"'.";
    let c: char = 'Ф';
    let r: str = r##" raw string "##;

    let result = 1, i = 1;
    while i <= n { // No parens around the condition
        result *= i;
        i += 1;
    }
    ret result;
}

pure fn pure_length<T>(ls: list<T>) -> uint { /* ... */ }

type t = map::hashtbl<int,str>;
let x = id::<int>(10);

// Define some modules.
#[path = "foo.rs"]
mod foo;

impl <T> Seq<T> for [T] {
    fn len() -> uint { vec::len(self) }
    fn iter(b: fn(T)) {
        for elt in self { b(elt); }
    }
}

enum list<T> {
    Nil;
    Cons(T, @list<T>);
}

let a: list<int> = Cons(7, @cons(13, @nil));

struct Baz<'a> {
    baz: &'a str,
}

'h: for i in range(0,10) {
    'g: loop {
        if i % 2 == 0 { continue 'h; }
        if i == 9 { break 'h; }
        break 'g;
    }
}

```

## scala

```scala
case class Person(name: String, age: Int)

def absoluteValue(n: Int): Int =
  if (n < 0) -n else n

val hux = "hux"
def mux = "mux"
def qux: String = "qux"

type ξ[A] = (A, A)

trait Hist { lhs =>
  def ⊕(rhs: Hist): Hist
}

def gsum[A: Ring](as: Seq[A]): A =
  as.foldLeft(Ring[A].zero)(_ + _)

sealed trait Compass
case object North extends Compass
case object South extends Compass
case object East extends Compass
case object West extends Compass

trait Cake {
  type T;
  val things: Seq[T]

  abstract class Spindler

  def spindle(s: Spindler, ts: Seq[T], reversed: Boolean = false): Seq[T]
}

val colors = Map(
  "red" -> 0xFF0000,
  "turquoise" -> 0x00FFFF,
  "black" -> 0x000000,
  "orange" -> 0xFF8040,
  "brown" -> 0x804000)

lazy val ns = for {
  x <- 0 until 100
  y <- 0 until 100
} yield (x + y) * 33.33

```

## scss

```scss
@import "compass/reset";

// variables
$colorGreen: #008000;
$colorGreenDark: darken($colorGreen, 10);

@mixin container {
    max-width: 980px;
}

// mixins with parameters
@mixin button($color:green) {
    @if ($color == green) {
        background-color: #008000;
    }
    @else if ($color == red) {
        background-color: #B22222;
    }
}

button {
    @include button(red);
}

div,
.navbar,
#header,
input[type="input"] {
    font-family: "Helvetica Neue", Arial, sans-serif;
    width: auto;
    margin: 0 auto;
    display: block;
}

.row-12 > [class*="spans"] {
    border-left: 1px solid #B5C583;
}

// nested definitions
ul {
    width: 100%;
    padding: {
        left: 5px; right: 5px;
    }
  li {
      float: left; margin-right: 10px;
      .home {
          background: url('http://placehold.it/20') scroll no-repeat 0 0;
    }
  }
}

.banner {
    @extend .container;
}

a {
  color: $colorGreen;
  &:hover { color: $colorGreenDark; }
  &:visited { color: #c458cb; }
}

@for $i from 1 through 5 {
    .span#{$i} {
        width: 20px*$i;
    }
}

@mixin mobile {
  @media screen and (max-width : 600px) {
    @content;
  }
}
```

## sql

```sql
BEGIN;
CREATE TABLE "topic" (
    -- This is the greatest table of all time
    "id" serial NOT NULL PRIMARY KEY,
    "forum_id" integer NOT NULL,
    "subject" varchar(255) NOT NULL -- Because nobody likes an empty subject
);
ALTER TABLE "topic" ADD CONSTRAINT forum_id FOREIGN KEY ("forum_id") REFERENCES "forum" ("id");

-- Initials
insert into "topic" ("forum_id", "subject") values (2, 'D''artagnian');

select /* comment */ count(*) from cicero_forum;

-- this line lacks ; at the end to allow people to be sloppy and omit it in one-liners
/*
but who cares?
*/
COMMIT
```

## stylus

```stylus
@import "nib"

// variables
$green = #008000
$green_dark = darken($green, 10)

// mixin/function
container()
  max-width 980px

// mixin/function with parameters
buttonBG($color = green)
  if $color == green
    background-color #008000
  else if $color == red
    background-color #B22222

button
  buttonBG(red)

#content, .content
  font Tahoma, Chunkfive, sans-serif
  background url('hatch.png')
  color #F0F0F0 !important
  width 100%

```

## swift

```swift
extension MyClass : Interface {
    class func unarchiveFromFile<A>(file : A, (Int,Int) -> B) -> SKNode? {
        let path: String = bundle.pathForResource(file, ofType: "file\(name + 5).txt")
        let funnyNumber = 3 + 0xC2.15p2 * (1_000_000.000_000_1 - 000123.456) + 0o21
        var sceneData = NSData.dataWithContentsOfFile(path, options: .DataReadingMappedIfSafe, error: nil)
        /* a comment /* with a nested comment */ and the end */
    }
    @objc override func shouldAutorotate() {
        return true
    }
}

```

## typescript

```typescript
class MyClass {
    public static myValue: string;
    constructor(init: string) {
      this.myValue = init;
    }
  }
  import fs = require("fs");
  module MyModule {
    export interface MyInterface extends OtherInterface {
      myProperty: any;
    }
  }
  declare magicNumber number;
  myArray.forEach(() => {
    // fat arrow syntax
  });
```

## xml

```xml
<?xml version="1.0"?>
<response value="ok" xml:lang="en">
  <text>Ok</text>
  <comment html_allowed="true"/>
  <ns1:description><![CDATA[
  CDATA is <not> magical.
  ]]></ns1:description>
  <a></a> <a/>
</response>


<!DOCTYPE html>
<title>Title</title>

<style>body {width: 500px;}</style>

<script type="application/javascript">
  function $init() {return true;}
</script>

<body>
  <p checked class="title" id='title'>Title</p>
  <!-- here goes the rest of the page -->
</body>

```

## Ruby

```ruby
require 'spec_helper'
require 'helper/account'
require 'ruby-box'

describe RubyBox::File do
  it "should use missing_method to expose files fields" do
    file = RubyBox::File.new(@session, @mini_file)
    file.id.should == '2631999573'
  end

  it "should load all meta information if reload_meta is called" do
    # request is called once when reload_meta is automatically executed.
    RubyBox::Session.any_instance.should_receive(:request).once.and_return(@full_file)
    session = RubyBox::Session.new

    file = RubyBox::File.new(session, @mini_file)
    file.size.should == 629644
  end
end
```

### erb

```erb
<ul>
  <% for @item in @items -%>
    <li><%= @item %></li>
  <% end -%>
</ul>
```
