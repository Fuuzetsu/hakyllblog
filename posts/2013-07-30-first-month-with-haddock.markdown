---
title: First month with Haddock
description: Overview of the first month of the Haddock GSOC project
author: Fūzetsu
---

As per the
[original proposal](https://gist.github.com/Fuuzetsu/81253ba7d0c51ac88052), over
the last couple of weeks I have been hacking on Haddock. I will talk a bit on
where we currently stand, what has been achieved, what hasn't and what's next.
While the proposal wasn't originally my idea, I will do my best to expose the
reasoning behind it.

Note: This is written in from of me talking about my personal experience with
Haddock. If all you want are some hard facts on what's implemented, what isn't,
what's planned, I recommend you skip to the [overview](#shortversion).
The [Tests](#tests) subsection might also interest you. This first post
is long because it outlines a large chunk of time. Future posts should be
considerably shorter and about a specific area of the project rather than
about the project as a whole.

The first couple of weeks were spent on reading the source. Unfortunately
Haddock is quite tightly bound to GHC which means that I had to do a lot of
GHC source diving as well just to get my grips on the situation. I recommend
reading the [GHC commentary](http://ghc.haskell.org/trac/ghc/wiki/Commentary) if
you haven't already as it's fairly informative. I think the main surprise to me
was that Haddock gets comment strings right from GHC. I guess that I always
thought that Haddock was simply ripping these right out of the source files.

Parser change
=============
The very first point of the proposal was to re-implement the existing parser
with a new, more powerful one. Alex and Happy are currently used to generate
the lexer and the parser at compile time. There are following problems:

* Both Alex and Happy work using grammars. While grammars are nice things to
use, they aren't very nice things to modify. We have to specify a grammar for
both, the lexer and the parser separately, and any changes we want to make have
often have to be reflected in both. Worse, the lexer and parser
grammars don't have a clear overlap in structure so it's difficult to
decide where the grammar has to change to accommodate our needs.

* Backtracking is rather limited. There were previously attempts to implement
auto-linking and from what I can gather, the implementation was too complex
to actually be worth the time and effort. With a new parser, this limitation
would be removed and we could freely look back at the just-parsed sections
and even run additional parsers on these.

With these two main points in mind, Attoparsec was chosen with hopes that parser
combinators will provide a cleaner, more maintainable and extendable
implementation. As a downside, it depends on Data.Text which is currently not in
in GHC boot library. Fortunately, it also
has a ByteString version which lets us avoid the problem.

A word of advice: if you ever move around modules in an existing project, or
even add some new ones, make sure to update your cabal file because all you're
going to get are weird linker errors at the end of the build with no indication
of what's wrong.

<a id="tests"></a>

Tests
-----
When time to code actually came and I got GHC HEAD going (this is unfortunately
a must for Haddock hacking. On an upside, you get all the cool HEAD stuff like
TypeHoles), I very quickly realised that running tests was a major pain. There
are two test suites: HTML tests and Hspec tests. HTML tests are just short
Haskell modules that we compile, generate documentation for and diff
with reference, known-good HTML files. This means going to the shell, compiling
haddock, running‘cabal test’, waiting for the other test-suite to run first and
examining output. Even worse, if anything is actually wrong, you don't even get
told which file it's in and you have to grep for the elements of the output.

Hspec tests are the nice kind. The kind that you can load into GHCi and whenever
you're ready, simply ‘:r’ and ‘main’ to get a coloured output and clear
indication of what's failing. The downside is that there were only about 5 cases
there which means that you often passed these only to fail on HTML tests later.
If at all possible, it'd be nice to fail early, before going out to the shell
and doing all the tedious stuff. My first remedy to this was less-than-pretty.

I added trace statements to the part of the program that takes in strings and
outputs our internal format (nested ‘Doc’ structure). With this, and a couple of
minutes with emacs macros, I soon had hundreds of (rather redundant)
Hspec tests without going out to the shell. I have later removed the
redundancies and split them up into nice categories. This way we actually had
some tests. I should note that there's one more type of testing I like to do and
that is to download existing, large libraries that compile both with HEAD and
stable, generating docs for them and then diffing the two. If this passes then
I feel fairly confident that nothing major got broken.
[HXT](http://hackage.haskell.org/package/hxt) was pretty good for this. Any
problems discovered this way would go right into Hspec tests which is how
the test-suite has naturally grown into ~75 reasonable test cases.

Implementation
==============
My first one-pass attempt failed horribly. I could not reproduce the behaviour
of the original program. A lot of the grammar seems to have been written for
Alex and Happy: that is, the syntax was designed for easy parsing to those
rather than coming up with the syntax first, independently of the tool used.

Defeated and already a fair amount of time into the coding stage, I have changed
my approach. The initial attempt at this resulted in what was very much a
grammar implementation using Attoparsec using some custom combinators for state
transition (for the lexing) and backtracking with lists (for the parser).
As you can imagine, this doesn't gain us much over the original program but the
hopes were that it will be easier to transition from this into a single pass
than it would be to do so straight away. While implementing a few new features
with this parser and writing few more tests for those, the time came where a
single-pass parser was necessary. I have met with the same problems as initially
(trying to combine two grammars into a single-pass parser combinator
implementation) but have recently received guidance which made a large amount
of these issues go away and made the implementation far more idiomatic in
respect to using parser combinator approach. This parser is nearly finished (and
probably would have been if I didn't choose to write this post instead). It's
worth noting that this parser will bring a couple of changes and will therefore
no longer be 100% compatible with the old one. More syntax will be allowed and
no old syntax will be disallowed which means that old documentation should be
safe to generate with this new parser without fear of breaking it.


While I have spent a huge majority of my time in the parser part of the program,
I suspect that features such as GADTs and type families which are scheduled to
be added in the future will take me well outside this area, potentially into the
GHC source again. You can expect blog posts about those if they turn out to be
interesting enough to post about.

<a id="shortversion"></a>

Overview
========
Note that everything denoted below is not yet in the Haddock tree.
Announcement will be made when the changes are pushed upstream.

* A 100% compatible parser has been written. This unfortunately
suffers from poor implementation and split into a lexer and parser.

* New, single-pass parser (Attoparsec) nearly complete. As of writing, there are
5 more test cases failing. This is simply because I started writing this version
about 20 hours ago and it's missing some features. Should be done very soon.
This parser no longer has the same behaviour as the original, extending and
changing the syntax and its rules. All old documentation should render as it did
before unless it happens to hit any of the new additions. It should not be an
issue at all.

* Tens of new test cases were added. Travis CI was set up to help with the
testing but is currently not running due to lack of GHC HEAD binaries. It is
unreliable to try and build GHC HEAD during the testing stage.

* Couple of no-longer-relevant on Haddock Trac were closed.

* List entries no longer have to be separated by empty lines. Note that
different kinds of lists *do* have to be separated by empty lines, by design.

* Not preceding an example or birdtracks by an empty line block is no longer a
parse error: they will simply be rendered as regular strings.

* Opening a definition list with a square bracket and not closing it on the same
line (therefore forming a valid definition list) is no longer a parse error.
The square bracket will be treated as a regular character and will not need
to be escaped.

* You can now specify titles for your images as you would with URLs. The old
`<<foo.png>>` image syntax now denotes an image without a title.
`<<foo.png bar>>`
denotes an image ‘foo.png’ with a title ‘bar’. Changes to the HTML and LaTeX
backends were made to accommodate this. The markup generated for images without
a title is identical to what it was before (no empty `title` attributes).

* Naive URL auto-linking is now in place. Any text starting with `ssh://`,
`http://`, `https://`, `ftp://` or `gopher://` until a first white space
character will be treated as a URL. It is not possible to give such URL a title.
No validation is done on the URL, to keep the parser simple (but it has been
coded and if this simple approach proves problematic, might be put in) save for
splitting trailing single-character punctuation. This is only in effect wherever
URLs with the `<example.com title>` syntax are accepted.

* Module names (that is, strings between double quotes) are now only accepted if
they clearly aren't syntactically incorrect. That is, enclosing strings that
contain spaces or don't start with a capital letter in double quotes will no
longer result in a hyperlink being created. Note that these aren't checked for
actual validity or existence by GHC so links to non-existent modules are still
possible.

Plans
-----
* GADT support is still planned as per the proposal.

* Markdown support is still planned as per the proposal.

* Bold text

* The module header will no longer require that the fields are in strict order.
If you know why this limitation was imposed save for implementation convenience,
please [contact me](/contact.html).

* The proposal states support for type families will be implemented. While I
don't have plans to back out of this, there was some code committed by GHC HQ
recently to do with type families. I have not reviewed the code carefully yet
but I believe it makes this job easier.

* The proposal states that the image tags should be made relative and that flags
should be created to allow us to provide these separately. I have to admit that
this is of lower priority to me than other tasks and I haven't heard much (or
any) demand for this feature.

* Documentation has to be updated. All the new features have to be documented
and even some old features which still aren't (such as the `<<img>>` tag) and
uploaded on haskell.org.

Any syntax extensions are unlikely to break any existing documentation.
