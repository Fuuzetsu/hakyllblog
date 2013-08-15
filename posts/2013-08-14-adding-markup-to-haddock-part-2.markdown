---
date:   2013-08-15 02:06:08
title:  Adding bold to Haddock (part 2)
author: Fūzetsu
---

A while ago,
[I wrote](/posts/2013-08-05-adding-markup-to-Haddock.html) about
adding support for __bold__ to Haddock. Last time, I added tests and
made the initial implementation. Now I'm going to briefly go over
which files one needs to change for the back-ends.

There are three back-ends: LaTeX, Hoogle and XHtml. XHtml is the one
people see the most, I imagine. There are no tests for either Hoogle
nor LaTeX and I suspect they might have seen a fair amount of breakage
already. I haven't heard direct complaints though so I'll look into
verifying these later.

First in `Utils.hs` we add a new default field called `markupBold` to
the `idMarkup` identity record and immediately after we add a new
pattern covering `DocBold` to the `markup` function. This is used
throughout the back-ends as a uniform interface. We also need to add a
pattern for `DocBold` in the `renameDoc` function `Rename.hs`. From
what I gather from the very scarce comments, this module renames
things such as identifiers into a more human-friendly form: there's no
need to render something as `Foo.Bar.Baz` if we're in `Foo.Bar`.
Unfortunately, it's 500 LOC of juggling of GHC's types so I can't be
certain. Also add the new pattern to the `rename` function in
`LexParseRn.hs` which runs the actual look-up.

Updating the interfaces is quite simple in this case. In each of

```
 src/Haddock/Backends/Hoogle.hs
 src/Haddock/Backends/LaTeX.hs
 src/Haddock/Backends/Xhtml/DocMarkup.hs
```

find the function where other markup is translated and add your
change. For example for LateX, it was a 3 line change:

```{.diff}
   markupEmphasis             = \p v -> emph (p v),
+  markupBold                 = \p v -> bold (p v),
   markupMonospaced           = \p _ -> tt (p Mono),

-- snip…

+bold :: LaTeX -> LaTeX
+bold ltx = text "\\textbf" <> braces ltx

```

It's easy to look around and see how things are done and mimic the
behaviour. Last but not least, there's a `InterfaceFile.hs` which
unsurprisingly deals with Haddock's interface file. This file can be
used later by Haddock to link against packages it has already
generated documentation for. Look at the `Binary` instance for `Doc
id` and change it accordingly. __If you change the instance, make sure
to bump the `binaryInterfaceVersion`__. Also update the
`binaryInterfaceVersionCompatibility`. This will ensure that we get a
nice error message if we try to link between incompatible versions
rather than weird behaviour. This file is only relevant if you
add/remove markup or structurally change existing one. Simple parser
changes to existing markup do not affect this file.

I am not bumping this until everything is finalised and ready for
release but I have to be careful to not let any test docs I generate
with it get out of the sandbox.

We're done, both test-suites pass. Here's a list of changes I had
to make all together. The majority is just the tests, with very few
actual code changes (and some of it is just clobbering whitespace &c).

```
12 files changed, 170 insertions(+), 12 deletions(-)
 html-test/ref/Bold.html                 | 102
 html-test/src/Bold.hs                   |   9
 src/Haddock/Backends/Hoogle.hs          |   7
 src/Haddock/Backends/LaTeX.hs           |   3
 src/Haddock/Backends/Xhtml/DocMarkup.hs |   1
 src/Haddock/Interface/LexParseRn.hs     |   1
 src/Haddock/Interface/Rename.hs         |   7
 src/Haddock/InterfaceFile.hs            |   6
 src/Haddock/Parser.hs                   |  13
 src/Haddock/Types.hs                    |   3
 src/Haddock/Utils.hs                    |  11
 test/Haddock/ParseSpec.hs               |  19
```

It's time for some images. What use is all this if it doesn't look
pretty in the end? Here's the result of the efforts. Generating docs
for the following code

```haskell
-- | Module : File
module File where

-- | /SomeType/
data SomeType

-- | __Othertype__
data OtherType

-- | Here's some __bold__
foo :: SomeType -> SomeType
foo = undefined

-- | __Multi-word bold__
bar :: OtherType -> OtherType
bar = undefined

-- | __No multi-line
-- bold, no sir__
baz :: [a] -> [a]
baz = undefined

-- | __Can't escape \\__ the underscores__
qux :: SomeType -> OtherType
qux = undefined

-- | __Can't even have a single unescaped _ in the string__
quux :: t
quux = undefined

-- | __No other /markup/ inside either__
corge :: OtherType -> SomeType
corge = undefined
```

yields us

![naive bold implementation](/images/poor_bold.png)


Something that I wanted to do for a while and that was further
motivated by a
[recent Trac ticket](http://trac.haskell.org/haddock/ticket/252) was
to allow markup inside of emphasis (and now bold). There's also a
[much less recent ticket](http://trac.haskell.org/haddock/ticket/126)
about multi-line emphasis (and now bold). Here's an exclusive preview
of both of these features. In fact, even I'm actually rendering the
documentation for the first time to inspect with my eyes as I rely on
tests otherwise:

![markup inside markup](/images/inside_markup.png)

Also, something I'm less enthusiastic about, multi-line markup:

![multi-line bold](/images/multi_markup.png)

See the first ticket I linked to for reasoning. I'm writing
[a tool](http://hackage.haskell.org/package/doccheck) on a side to help
to determine the effects of various changes on existing documentation
but it is not usable it. It's difficult to reliably extract Haddock
comments from thousands of files without actually building the
projects. I'm thinking of using
[haskell-src-exts](http://hackage.haskell.org/package/haskell-src-exts)
to help and make this task easier but there's
[a problem](http://comments.gmane.org/gmane.comp.lang.haskell.cafe/106768)
with this approach as well. Currently I compensate for the lack of
such tool with tests.

I might write a post in the future on the progress of this and if and
how the problems were solved (or weren't solved!).
A warning system enabled with a flag might be nice in Haddock itself
but this is not currently planned.


As a side note, I had problem with my e-mail address from 4th to 14th
of August, so if you tried to [contact me](/contact.html) and didn't
get a reply, please try again.
