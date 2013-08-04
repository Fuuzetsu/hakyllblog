---
title: Adding bold to Haddock (part 1)
author: Fūzetsu
---

Today I'm going to talk about and go through implementing __bold__
support to Haddock. I am going to be writing this as I implement it,
hopefully exposing the relevant parts and order in which I do so.

The aim of this post is to document the process and to serve as a reference
to anyone in the future wishing to add such constructs, saving them
time from looking what to change and where. This post will cover
adding the new element to the parser, generating tests and making the
new element pass the Hspec tests. Back-end changes will be covered in
a future post.

Adding the type
----------------
First we need to add the appropriate type to Haddock. Haddock comments
are all parsed into a Doc type, conveniently residing in
`Haddock.Types`.

```haskell
data Doc id
  = DocEmpty
  | DocAppend (Doc id) (Doc id)
  | DocString String
  | DocParagraph (Doc id)
  | DocIdentifier id
  | DocIdentifierUnchecked (ModuleName, OccName)
  | DocModule String
  | DocWarning (Doc id)
  | DocEmphasis (Doc id)
  | DocMonospaced (Doc id)
  | DocUnorderedList [Doc id]
  | DocOrderedList [Doc id]
  | DocDefList [(Doc id, Doc id)]
  | DocCodeBlock (Doc id)
  | DocHyperlink Hyperlink
  | DocPic Picture
  | DocAName String
  | DocProperty String
  | DocExamples [Example]
  deriving (Functor)
```
I'll just add `DocBold (Doc id)` and be on my way.

The next logical step would be to add some tests. As I wrote before,
there are two test suites: [Hspec](http://hspec.github.io/) tests,
which are nice, quick to run, and test that the chunks of text parse
into the Doc we want, and there are HTML tests which make sure that
whole files parse into what we want (acting as XHTML back-end tests).
Currently there are no tests for the LaTeX back-end.

In `test/Haddock/ParseSpec.hs`, I'll quickly add a few rather trivial
tests that are consistent with other markup. This is effectively where
you have to decide on the behaviour you want to get out of your new
construct as well as the delimiters. I decided for bold to use
double-underscores, `__`, as opposed to `*` or a single `_` because

* It's consistent with pretty much all common  Markdown flavours as
  can be seen on John MacFarlane's brilliant
  [Babelmark 2](http://johnmacfarlane.net/babelmark2/?text=__hello+world__%0A%0A*hello+world*%0A%0A_hello+world_)

* Double underscores are far less common in regular text. Escaping `*`
  and `_` everywhere would get really tedious, really fast.

Hopefully any bike-shedding can be avoided although don't hesitate to
[contact me](/contact.html) if you have some great reasons for why it
shouldn't be `__`.

So with that, and a quick use of
[multiple-cursors](https://github.com/magnars/multiple-cursors.el) on
existing emphasis tests, we now have `102 examples, 4 failures`.
Before I proceed, HTML tests are in order as back-ends have to be
updated for new elements, else they won't know how to render them. We
have to put some Haskell source files containing Haddock comments with
bold in `html-test/src` and the expected XHTML source in
`html-test/ref`. The files generated during the testing are put in
`html-test/out`.

In this case we are lucky: emphasis is already implemented and has the
same rules for where it can be position. This means we can simply
write the test with emphasis, have Haddock generate the file with the
right structure for us and then change all the `<em>` tags to
`<strong>` tags without much fear of breaking anything.

So we start with this:

![Simple emphasis doc](/images/bold_raw.png)

and get out a HTML file that reflects the structure we want. Just change the
tags in the `out` file, move it to `ref` and update our test case doc.

![Updated doc](/images/bold_test.png)

and with this we get a (not so nice) failing HTML test diff:

    64,66c64
    < 	    >Some <strong
    < 	      >bold text</strong
    < 	      >.
    ---
    > 	    >Some __bold text__.
    70,72c68
    < 	      > <strong
    < 		>Bold</strong
    < 		> in a list
    ---
    > 	      > __Bold__ in a list
    77,79c73
    < 	      ><strong
    < 		>bold in a definition</strong
    < 		></dt
    ---
    > 	      >__bold in a definition__</dt
    85,89c79
    < 	    > bold <strong
    < 	      >in</strong
    < 	      > a <strong
    < 	      >code</strong
    < 	      > block</pre
    ---
    > 	    > bold __in__ a __code__ block</pre


Implementation
---------------
This time we'll only make changes in the parser. Back-end changes will
be covered in the future.

Let us jump to the parser first. As I mentioned before, __bold__ is
very similar to *emphasis* in where it can appear. For reference,
here's code from the current emphasis element parser

```haskell
    -- | Emphasis parser.
    -- 
    -- >>> parseOnly emphasis "/Hello world/"
    -- Right (DocEmphasis (DocString "Hello world"))
    emphasis :: Parser (Doc RdrName)
    emphasis = DocEmphasis . DocString . decodeB
               <$> ("/" *> takeWhile1 (`notElem` "/\n") <* "/")
```
With this spell in hand, let's try to write a parser for bold:

```haskell
    -- | Bold parser.
    --
    -- >>> parseOnly bold "/Hello world/"
    -- Right (DocBold (DocString "Hello world"))
    bold :: Parser (Doc RdrName)
    bold = DocBold . DocString . decodeB
           <$> ("__" *> takeWhile1 (`notElem` "_\n") <* "__")
```
There's a rather obvious problem with this (and the emphasis parser):
we can't have bold text with an underscore inside of it, even if you
were to escape it (escaping strings is done during parsing and not as
post-processing). Unfortunately, this is to keep consistent with the
old parser: it will eat up everything as a plain string until the
first character of its delimiter. You can see this behaviour easily if
you try to use `]` in a title of a definition list or try to include
`>` in a URL that uses the `<url>` syntax. I am considering changing
this behaviour but not until I can gather more information about
already existing documentation and whether any of it would change
because of this. Perhaps I'll even make a blog post with pretty pictures
and diagrams.

Next we actually include the parser in the same places where emphasis
is allowed:

```haskell
p :: Parser (Doc RdrName)
p = mconcat <$> some (charEscape <|> monospace d <|> anchor <|> identifier d
                      <|> moduleName <|> picture <|> url <|> bold
                      <|> emphasis <|> encodedChar <|> string' <|> skipChar)
```

where `p` is just a function bound in a `where` inside of a larger
parser.

Let's run Hspec tests.

```
102 examples, 0 failures
```

Awesome. With this we need to dive into the back-ends, which will be
covered in a future post. Look forward to it.
