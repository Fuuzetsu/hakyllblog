---
date:   2014-03-24 15:33:16
title:  New Haddock released! A visual guide to changes.
author: Fūzetsu
---

We've just uploaded [Haddock 2.14.1][haddockhackage] and while you can
view the [CHANGES][changelog] file, here I'll attempt to present all
new features added since 2.13.2.1. A quick note that while 2.14.0 is
in the [CHANGES][changelog] file, it was never officially released to
the public. Consider it an internal release if you will. This
basically covers 2.14.0 and 2.14.1. I am posting this now as I hear
GHC 7.8.1 is supposed to come out in a few hours and this is the
version that you'll be getting. I had only just realised this but this
integrates the changes I have made over the last GSoC into a stable
GHC release. FYI, I'm using GHC 7.8-rc2 for any code snippets
presented here. Last thing to mention is that any ticket numbers you
see here are the tickets as seen on [Haddock Trac][trac]. We're
actually planning to move everything to GitHub soon so keep that in
mind if you're reading this further in the future. Note that pretty
much everything here is described in
[Haddock documentation][haddockdocs] (although without nice examples)
so please refer to that if you need further information.

Let's begin!

* Print entities with missing documentation (#258)

    This adds a `--print-missing-docs` flag to Haddock. Given a file
    like this:

    ```haskell
    module Foo where

    f :: ()
    f = ()

    -- | Doc for 'g'
    g :: ()
    g = ()

    class FooClass a where
    ```

    we can ask Haddock to tell us which docs are missing:

    ```
    $ haddock Foo.hs -h -o /tmp --print-missing-docs
    Haddock coverage:
      25% (  1 /  4) in 'Foo'
      Missing documentation for:
        Module header
        f (Foo.hs:3)
        FooClass (Foo.hs:10)
    ```

    There has been a suggestion to make this flag default. I'm
    personally not against it. What do you think?

* Print a warning message when given `-optghc` instead of `--optghc`
  (#5)

    This is just a quick fix to a long-standing feature request. The
    problem was that `-optghc` actually means `--odir=ptghc` which is
    probably not what you wanted. We now warn when we see `-optghc` in
    the flags. The warning is:

    ```Warning: `-optghc' means `-o ptghc', did you mean `--optghc'?```

* Add `--compatible-interface-versions` (#231)

    This simply prints the versions of the .haddock interface files
    that your Haddock binary knows how to work with.

    ```
    $ haddock --compatible-interface-versions
    25
    ```

    We had some fairly big changes to the interface file so current
    Haddock can only work with a single version: this means it can't
    re-use .haddock files that your previous versions might have
    generated.

* Allow to generate latex documentation for FFI declarations (#247)

    Fairly self-explanatory. Note that I don't encourage actually
    trying to use the LaTeX back-end, it is not maintained and has
    many bugs. It is meant to serve a sole purpose of generating the
    Haskell Report when that time comes. If you are interested in
    using this back-end and are willing to put in some time to breathe
    some life into it, we'd love to have you, contact us!

* Add copyright and license information to generated documentation

    We let you document modules with a comment containing some special
    fields. The header is documented [here][headerdoc]. Consider the
    following module:

    ```haskell
    {-|
    Module      : W
    Description : Short description
    Copyright   : (c) Some Guy, 2013
                      Someone Else, 2014
    License     : GPL-3
    Maintainer  : sample@email.com
    Stability   : experimental
    Portability : POSIX

    Here is a longer description of this module, containing some
    commentary with @some markup@.
    -}
    module W where
    ```

    Here's how it renders using 2.13.2:

    ![Old module info box]

    and here is how it renders with 2.14.1:

    ![New module info box]

    As you can see, perhaps copyright holders could be presented
    better. Perhaps in the next release each author will be on its own
    line, see ticket #279.

* Improved Unicode support

    Unicode support previously was very finicky. We now have a new
    parser which can handle unicode much better. Here's an example
    comment with a single definition list:

    ```haskell
    -- | [灼眼のシャナ] ℕ ℤ ℚ
    f :: ()
    f = ()
    ```

    Here's how 2.13.2 renders it:

    ![Old unicode rendering]

    and here's how 2.14.1 renders it:

    ![New unicode rendering]

    Much better! Notice a character missing in the old rendering.

* Bold markup support

    I have covered this one in the past so here's only a brief
    mention. Double underscores are used to denote that something is bald.

    ```haskell
    -- | This is bold: __Hello world. Underscores_are_allowed__
    f :: ()
    f = ()
    ```

    ![Bold support]

    Note that just like with other such markup (emphasis), we do not
    allow the user to stretch it over multiple lines.

    ```haskell
    -- | This is not bold: __Hello world.
    -- Underscores_are_allowed__
    f :: ()
    f = ()
    ```

    ![No multiline support]

    This is by design. We feel that extra complexity of implementation
    and the fact that it changes how 2.13.2 behaved does not warrant
    such support. See ticket #126 for minor discussion.

* Nested paragraphs

    This is a pretty big addition and if you are the type of person
    that tries to format their comments so that they look nice in
    source, you'll probably need to pay attention. Basically, we allow
    something like what Markdown allows: nesting things under list
    elements (such as more list elements and so on). A simple example
    would be nesting some a code snippet and another list under some
    other list. I'm actually showing off two features here. Consider

    ```haskell
    {-|
    * This is some list

        @
        This
        is
        code
        @

        * Another list
        * Second element of inner list, not separated by line break.
    -}
    f :: ()
    f = ()
    ```

    2.13.2 makes a mess out of it:

    ![Old nested lists]

    but 2.14.1 does what you might expect:

    ![New nested lists]

    The rule is that everything to be nested under a list element is
    to be indented 4 spaces from the start of the comment. Note that
    this is *not* 4 spaces relative from start of the previous list.
    You also have to make sure that the nested paragraph is separated
    by a line break so that Haddock doesn't simply think it's the
    continuation of the list.

    A double nesting will therefore look like this:

    ```haskell
    {-|
    * Top level

        * First nested

            * Second nested
    -}
    f :: ()
    f = ()
    ```

    ![Twice nested]


    Those with sharp eyes will notice that I have two list elements
    not broken up by the line break in the initial example. This in
    now allowed as long as the list elements are of the same type:

    This is now fine:

    ```haskell
    -- |
    -- * foo
    -- * bar
    ```

    but this is *not* fine:

    ```haskell
    -- |
    -- * foo
    -- 1. bar
    ```

    Haddock will think it's just a single list element and it will
    look something like this:

    ![Different type no break]

    Please refer to [list section of the docs][nesteddoc] for details.
    These changes mean that you can write much nicer docs but they
    also mean that if you wrote something that wasn't exactly model
    Haddock before, it might now look radically different! I know that
    even GHC is guilty of this.

* Better escaping

    We now have much better escaping behaviour. Consider

    ```haskell
    -- | < http:/haskell.org Haskell.org loves \>\>= >
    ```

    2.13.2 messes up:

    ![Old link escape]

    But 2.14.1 works as we'd like it to:

    ![New link escape]

    It is actually impossible to have the `>` character in the link or
    alt text even with HTML escapes because we don't accept markup
    there so it won't get converted.

    If you don't need the alt text, we now even automatically try to
    convert text to links. Consider

    ```haskell
    -- | http://haskell.org is cool
    f :: ()
    f = ()
    ```

    2.13.2 doesn't do what we want at all and even swallows up the
    forward slashes because it thinks it sees (empty) emphasis:

    ![Old autolink]

    2.14.1 does something much more reasonable:

    ![New autolink]

    You should notice that escaping things is much more reasonable
    now.

* Header markup

    Headers in regular comments (rather than just for sections) are
    now allowed. The syntax is multiple `=` characters, from 0 up
    to 6. Each back-end decides how to render the different header
    levels itself.

    ```haskell
    {-|
    = Top level
    * Hello
    * World

    == Subheader
    === Subsubheader
    @More stuff!@
    -}
    f :: ()
    f = ()
    ```

    ![Headers]

    Note that headers have to be at the beginning of a paragraph but
    we do allow a paragraph to follow without a line break right after
    it. This allows you to write down things like lists and another
    header straight after.

* Parser should no longer fail to parse any markup

    We now aim to be able to parse everything. This means that you
    should never see a parse failure caused by bad Haddock syntax. For
    example

    ```haskell
    -- | [ hello
    f :: ()
    f = ()
    ```

    fails on 2.13.2 with a parse error: `doc comment parse failed:  [ hello`.
    This will render as you'd expect on 2.14.1:

    ![No parse error]

    This means that if you had a documentation that failed to parse
    due to such error before, it will now (silently) succeed.

    __Important__: please note that you can still have a different
    kind of parse error. If your comment is at a place where we don't
    expect it, that's an error. For example, the following will throw
    a parse error:

    ```haskell
    data F = F () -- ^ Doc for first ()
               () -- ^ Doc for second ()
    ```

    gives us ```W.hs:18:12: parse error on input ‘(’``` because we
    don't support documentation of each parameter to the constructors.

    Please do not report these as bugs! If you do get a `doc comment
    parse failed` then report that, you should not be seeing any of
    these anymore.

 * {-# OPTIONS_HADDOCK show-extensions #-} pragma will show the GHC extensions
   enabled in the module.

    I think this is a pretty nifty one. Consider

    ```haskell
    {-# LANGUAGE UnicodeSyntax #-}
    {-# LANGUAGE TypeFamilies #-}
    {-# LANGUAGE FunctionalDependencies #-}
    {-# LANGUAGE DataKinds #-}
    {-# LANGUAGE TypeOperators #-}
    {-# LANGUAGE FlexibleInstances #-}
    {-# OPTIONS_HADDOCK show-extensions #-}
    module W where
    ```

    You can now ask Haddock to list __all__ enabled extensions (even
    those implicit ones) with the Haddock pragma that I show above.
    This particular example renders like this:

    ![Ext pragma]

    If you have a Haskell98/2010/whatever pragma too, that will also
    get shown. Any extension implied by the current language
    (H98,2010) is not shown.

    I decided to show all the extensions, including the ones pulled in
    by stronger ones to discourage enabling the most powerful
    extensions without a good reason.

    This option is not a default. Do you think it should be?

* Properly render License field (#271)

    There was a bug where we rendered the wrong thing in the License
    field. I can't show you because it already has been patched up.
    I simply mention this for completeness.

* Print type/data family instances (for exported types only)

    Fairly self explanatory, your type/data family instances now get
    shown in the documentation.

    This example is a pretty big one because there's a fair amount of
    stuff going into it. This is actually a stripped down version used
    by Haddock for testing.

    ```haskell
    {-# LANGUAGE TypeFamilies, UndecidableInstances, PolyKinds, TypeOperators,
                 DataKinds, MultiParamTypeClasses, GADTs #-}
    module W where

    -- | Doc for: data X
    data X
      = X   -- ^ Doc for: X
      | XX  -- ^ Doc for: XX
      | XXX -- ^ Doc for: XXX

    -- | Doc for: data Y
    data Y

    -- | Doc for: class Test a
    class Test a

    -- | Doc for: instance Test X
    instance Test X
    -- | Doc for: instance Test Y
    instance Test Y

    -- | Doc for: type family Foo a
    type family Foo a :: k

    -- | Doc for: type instance Foo X = Y
    type instance Foo X = Y
    -- | Doc for: type instance Foo Y = X
    type instance Foo Y = X

    -- | Doc for: class Assoc a
    class Assoc a where
      -- | Doc for: data AssocD a
      data AssocD a :: *
      -- | Doc for: type AssocT a
      type AssocT a :: *

    -- | Doc for: instance Assoc X
    instance Assoc X where
      -- | Doc for: data AssocD X = AssocX
      data AssocD X = AssocX -- ^ Doc for: AssocX
      -- | Doc for: type AssocT X = Foo X
      type AssocT X = Foo X

    -- | Doc for: instance Assoc Y
    instance Assoc Y where
      -- | Doc for: data AssocD Y = AssocY
      data AssocD Y = AssocY -- ^ Doc for: AssocY
    ```

    and here's part of how it looks

    ![Type families]

* Fix display of poly-kinded type operators (#189)

    ![Old poly-kinded rendering]

    ![New poly-kinded rendering]

    We're still unsure how to display this to the user but at least
    now it's not completely wrong. Suggestions are most welcome,
    please comment on #189.

* PatternSynonyms support

    GHC 7.8 now has support for [pattern synonyms][patsyn]. Here's an
    example right from Haddock test-suite.

    ```haskell
    {-# LANGUAGE PatternSynonyms, PolyKinds, TypeOperators #-}

    -- | Testing some pattern synonyms
    module W where
    -- | FooType doc
    data FooType x = FooCtor x

    -- | Pattern synonym for 'Foo' x
    pattern Foo x = FooCtor x

    -- | Pattern synonym for 'Bar' x
    pattern Bar x = FooCtor (Foo x)

    -- | Pattern synonym for (':<->')
    pattern x :<-> y = (Foo x, Bar y)

    -- | Doc for ('><')
    data (a :: *) >< b = Empty

    -- | Pattern for 'Empty'
    pattern E = Empty
    ```

    The rendering is still pretty experimental so suggestion welcome!

    ![Pattern Synonyms]

* Fix display of implicit parameters (#260)

    ```haskell
    {-# LANGUAGE RankNTypes #-}
    {-# LANGUAGE ImplicitParams #-}
    module W where

    data Configuration

    c :: String -> ((?configuration :: Configuration) => IO b) -> IO b
    c = undefined
    ```

    ![Broken implicit params rendering]

    ![Fixed implicit params rendering]

* Fix rendering of Contents when links are present (#276)

    Given

    ```haskell
    module W where

    -- * Section header with 'f' link

    -- | f doc
    f :: ()
    f = ()
    ```

    We used to have a problem where a link in the header would break
    the Contents box rendering.

    ![Old contents]

    That is now fixed. Note that you can no longer click on ‘f’ in the
    Contents box to be taken there. I feel that it's the expected way.

    ![New contents]

* Fix documentation duplication on record fields (#195)

    I think this is going to be a pretty controversial one. Consider

    ```haskell
    module W where

    data F = FOne { field :: () -- ^ Doc for FOne field
                  }
           | FTwo { field :: () -- ^ Doc for FTwo field
                  }
    ```

    As ‘field’ is actually the same function, in the past Haddock
    would join the comments (it's in the weird order due to an unfixed
    bug):

    ![Old record doc rendering]

    We now instead take the doc of the first field to occur. Note that
    is used even if the first field has no comment and others do.

    ![New record doc rendering]

    See ticket #195 if you want to discuss this change. Both
    behaviours are weird but I think no one intentionally used the old
    behaviour.

* Add `--source-entity-line` for exact line links (eg. things defined
  inside TH splices) (#79)

    This allows HsColour to insert anchors for TH declarations.
    Nothing to show here, check the ticket for details.

* Display fixity information for names with nonstandard fixities

    There's no a mechanism in place to display fixity of (type) operators and
    infix functions. Includes exotic things like type families and
    pattern synonyms. Code omitted but there's nothing special you
    have to do, your new docs should automagically have fixities
    shown.

    ![Fixity rendering]

* Bird tracks specified like "> code" no longer suffer from an extra
  leading space in the code output

    Pretty self explanatory. We strip a leading space from code blocks
    generated by bird tracks.

    ```haskell
    -- |
    -- > hello
    -- > world
    f :: ()
    f = ()
    ```

    ![Old bird tracks]

    ![New bird tracks]

    This is also planned for the ‘@’ style code blocks which should
    have this implemented in the next Haddock release, most likely
    2.15.0 coming out with GHC 7.8.2.

* Render * and -> with their UnicodeSyntax equivalents if -U is
  enabled

    Replaces * and -> in extra places compared to 2.13.2.

    ![Old unicode syntax]

    ![New unicode syntax]

* Display minimal complete definitions for type classes

    I feel this is a nice feature.
    [GHC now supports MINIMAL pragmas][minimal] and we are now able to
    display it in the docs. Another example right out of the Haddock
    test-suite:

    ```haskell
    module W
      ( Foo(..)
      , Weird(..)
      , NoMins(..)
      , FullMin(..)
      , PartialMin(ccc)
      , EmptyMin(..)
      ) where

    class Foo a where
      -- | Any two of these are required...
      foo, bar, bat :: a

      -- | .. or just this
      fooBarBat :: (a,a,a)

      {-# MINIMAL (foo, bar) | (bar, bat) | (foo, bat) | fooBarBat #-}

    class Weird a where
      a,b,c,d,e,f,g :: a

      {-# MINIMAL ((a, b), c | (d | (e, (f | g)))) #-}

    class NoMins a where
      x,y,z :: a

      -- | Has a default implementation!
      z = x

    class FullMin a where
      aaa,bbb :: a

    class PartialMin a where
      ccc,ddd :: a

    class EmptyMin a where
      eee,fff :: a
      eee = fff
      fff = undefined
    ```

    ![Minimal pragma]

    Again I ask you to ignore the silly ordering of some grouped
    functions, this is the aforementioned old bug. Hopefully we can
    fix it by the next release.

* Hide right hand side of TF instances with hidden names on the RHS

    Changes a bit which TF RHSs are hidden. It is a change between
    2.14.0 and 2.14.1 and is only mentioned for completeness.


This is it for all the changes I can think of but I'm sure I missed
something! There was some other minor stuff fixed up that doesn't
deserve a mention on its own (such as fixing bullet point rendering in
constructor docs, #281) so I encourage you to read the commit history
if you need to know all the little details.

While I'd love to end it here, I do have to admit that there's a
regression in this release which we don't get to fix until GHC 7.8.2.

Namely, if you have a (very common) comment like this:

```haskell
-- |
-- @
-- some code
-- goes here
-- @
f :: ()
f = ()
```

2.13.2 will render it like this:

![Old codeblock rendering]

and 2.14.1 like this:

![New codeblock rendering]

The problem is that while Haskellers are used to putting a space after
the comment marker `--`, that space is actually a part of a comment
and we end up with an extra ‘empty’ line which actually has a single
space in front of it. This is the line with the closing @ on it.

All of the following let you workaround the problem:

```haskell
-- |
-- > some code
-- > goes here
f :: ()
f = ()

-- |
--@
--some code
--goes here
--@
g :: ()
g = ()

{-|
@
some code
goes here
@
-}
i :: ()
i = ()
```

Surprisingly, that second form is allowed. If you care a lot about the
extra line, please use a workaround for now and I'm sorry! If you
don't care that it looks a bit on the ugly side for a while, we'll
have a fix in the next release, most likely to ship with GHC 7.8.2.

Thanks!

[haddockhackage]: http://hackage.haskell.org/package/haddock-2.14.1
[changelog]: http://www.haskell.org/haddock/CHANGES.txt
[trac]: http://trac.haskell.org/haddock/
[headerdoc]: http://www.haskell.org/haddock/doc/html/ch03s03.html
[Old module info box]: /images/oldinfobox.png
[New module info box]: /images/newinfobox.png
[Old unicode rendering]: /images/oldunicoderendering.png
[New unicode rendering]: /images/newunicoderendering.png
[Bold support]: /images/newbold.png
[No multiline support]: /images/notbold.png
[haddockdocs]: http://www.haskell.org/haddock/doc/html/index.html
[nesteddoc]: http://www.haskell.org/haddock/doc/html/ch03s08.html#idp1371090476
[Old nested lists]: /images/oldnested.png
[New nested lists]: /images/newnested.png
[Twice nested]: /images/twicenested.png
[Different type no break]: /images/differentbreak.png
[Old link escape]: /images/oldlinkescape.png
[New link escape]: /images/newlinkescape.png
[Old autolink]: /images/oldautolink.png
[New autolink]: /images/newautolink.png
[Headers]: /images/headers.png
[No parse error]: /images/noerror.png
[Ext pragma]: /images/extpragma.png
[Type families]: /images/typefams.png
[Old poly-kinded rendering]: /images/oldpoly.png
[New poly-kinded rendering]: /images/newpoly.png
[patsyn]: https://ghc.haskell.org/trac/ghc/wiki/PatternSynonyms
[Pattern Synonyms]: /images/patsyn.png
[Broken implicit params rendering]: /images/oldimp.png
[Fixed implicit params rendering]: /images/newimp.png
[Old contents]: /images/oldcont.png
[New contents]: /images/newcont.png
[Old record doc rendering]: /images/oldrecord.png
[New record doc rendering]: /images/newrecord.png
[Fixity rendering]: /images/fixities.png
[Old bird tracks]: /images/oldbird.png
[New bird tracks]: /images/newbird.png
[Old unicode syntax]: /images/oldunicode.png
[New unicode syntax]: /images/newunicode.png
[minimal]: https://ghc.haskell.org/trac/ghc/ticket/7633
[Minimal pragma]: /images/miniprag.png
[Old codeblock rendering]: /images/oldblock.png
[New codeblock rendering]: /images/newblock.png
