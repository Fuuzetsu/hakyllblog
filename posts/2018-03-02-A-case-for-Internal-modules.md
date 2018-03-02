---
title: A case for .Internal modules.
author: Fūzetsu
---

Many libraries expose `.Internal` modules: these are module that
contain functions that are unsafe, unstable, with no guarantees and
quite often, fast. This is great if you Know What You Are Doing™.

Sadly, not all package authors seem to do this. Part of the motivation
is that in principle, it shouldn't be needed. Indeed this is what I
was told by a co-worker [on this seemingly unrelated rules_haskell
issue](https://github.com/tweag/rules_haskell/issues/152#issuecomment-367712602).
After a brief chat on Slack however, I was able to convince him
otherwise. Below is a paraphrasing of my arguments and reason why I
think `.Internal` modules are a good thing and you should be exposing
them. Find below some ramblings on why you should expose internals of
your libraries.

## Expose the internals, please.

1. I love your library but it can't cover all my use-cases. It's wrong
   for it to try. Please let me use the back doors.

   This is extremely common. I'm using your library and it's going
   great. I write my program and it runs fast. I look through
   [perf](http://www.brendangregg.com/perf.html) report and GHC's
   profiling output. It could be faster. I know how to make it faster.
   But I can't make it faster because I'm relying on your library
   which insists on taking the slow route.

   This is rarely the fault of the library. Libraries provide
   abstractions, data types, functions on those types and give us
   guarantees. To preserve the invariants, it needs to protect itself
   from the dumb users.

   I will give the same example that I gave when I initially discussed
   this. I was writing a small program recently and using a min priority
   queue. I was also tracking the maximal element that was inserted:
   something the library could not offer me in constant time and nor
   should it. When I wanted to insert this maximal element into the
   queue however, I have a problem: the library will perform linear in
   the size of the queue number of comparisons; it has to know where
   to insert the element after all. So [I created an issue in
   pqueue](https://github.com/lspitzner/pqueue/issues/18) library to
   let me do this. The easiest way is to expose the data type
   constructors in an `.Internal` module and let me do the traversal
   myself without doing the comparisons.

   Notably I want to stress that this is of no fault of the library:
   inserting arbitrary element in arbitrary place breaks the queue. It
   is only with my external knowledge that I am able to insert it
   safely.

   Could better library design have stopped this? I want to argue that
   no if we only have finite amount of time to spend. If we were using
   something like Agda or even just more esoteric parts of Haskell, we
   could actually improve the library to expose such a function. We
   could have `insertMaximal` that requires proof that the element is
   maximal. I could produce it. The library is not about providing
   this, would likely make the common case API more awkward and is
   generally probably not the best use of the author's time. Just give
   me the constructors, please.

2. I love your library and you have a function that does the exact
   thing I need already but… it's not exported.

   Very frequently library authors will implement certain things for
   their own convenience and only export a nice API to the user. This
   is OK if the API works for you but if something is missing, it's
   incredibly frustrating to find that the exact code you need already
   exists. Frequently it's not even unsafe code. I could give another
   `pqueue` example but let's look at a different one for variety.

   [text-show](https://hackage.haskell.org/package/text-show) is a
   package that provides a `Show`-like type class that instead of
   going to `String` or `String` builder (`String -> String`), uses
   `Text` instead. This is great because `String` is the devil and you
   shouldn't be using it. Recently I found myself wanting to save some
   memory so I reached for
   [ShortByteString](https://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString-Short.html#t:ShortByteString)
   which provides lower overhead than `ByteString`. The bytes I was
   storing were really just the ASCII character set and their raison
   d'être was to be printed to the terminal later if the user so
   requested. Can `text-show` help us? Sure, it has [a module with
   TextShow instance for
   ShortByteString](https://hackage.haskell.org/package/text-show-3.7.1/docs/TextShow-Data-ByteString.html).
   Oh, but hold on. It's not _quite_ what I want!

   ```haskell
   [shana@lenalee:~/hakyllblog]$ nix-shell -p 'haskellPackages.ghcWithPackages (p: [ p.text p.bytestring p.text-show ])' --run ghci
   GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
   Prelude> import Data.Text.IO
   Prelude Data.Text.IO> import Data.ByteString.Short
   Prelude Data.Text.IO Data.ByteString.Short> import TextShow
   Prelude Data.Text.IO Data.ByteString.Short TextShow> import TextShow.Data.ByteString
   Prelude Data.Text.IO Data.ByteString.Short TextShow TextShow.Data.ByteString> import TextShow
   Prelude Data.Text.IO Data.ByteString.Short TextShow TextShow.Data.ByteString> :set -XOverloadedStrings
   Prelude Data.Text.IO Data.ByteString.Short TextShow TextShow.Data.ByteString> Data.Text.IO.putStrLn (showt ("hello" :: ShortByteString))
   "hello"
   ```

   It's printing quotes around my content. I didn't want that. If we
   look at the
   [source](https://hackage.haskell.org/package/text-show-3.7.1/docs/src/TextShow-Data-ByteString.html#line-89),
   we can find that it defines `unpackChars :: ShortByteString ->
   [Char]` which does exactly what we need but then the instance looks
   like this:

   ```haskell
   instance TextShow ShortByteString where
    showb = showb . unpackChars
   ```

   Of course to my dismay, it does not export `unpackChars`, only the
   instance which is useless to me. My choice is to either copy and
   paste `unpackChars` into my own code or deal with it in different
   way such as going to `ByteString` and using a text builder function
   `ByteString -> Builder` which doesn't wrap with quotes and already
   exists.

   To add insult to the injury, `unpackChars` actually already exists
   in the `bytestring` package. Worse, `bytestring` even exposes
   `.Internal` modules already! Sadly the author decided to not expose
   that. This means `text-show` had to copy and paste the
   implementation and subsquentely so would I until someone had the
   foresight to export it. At least it is _possible_ to copy and paste
   this code because we have enough access to internal to re-implement
   it.

   Another offender of "does exactly what you want but it's inside a
   typeclass behind something you don't want" that I encountered
   recently is `store`. `Store` instances for `Vector` are generated
   through TH and they store vector length followed by the data. But I
   already knew the length from external source and just wanted the
   data. Solution? Well, two:

   1. newtype Vector passing the length through a type parameter with
      GHC.TypeLits or otherwise then retrieve the value in the
      implementation. Yuck and also slow (goes through `Integer` &c.)

   2. Don't use `Store` at all. Thankfully, `store-core` exists (thank
      you!) which lets you work on `Peek` directly. You still have to
      copy `-ddump-splices` to see the generate TH then copy and paste
      the part you're interested in. After adapting it a bit, I have a
      bunch of these things in my code for multiple vector types

      ```haskell
      peekVectorD :: Int -> Peek (VU.Vector Double)
      peekVectorD obs(I# tmo) = VUB.V_Double . VP.Vector 0 obs <$> Store.peekToByteArray "VU.Vector Double" blen
      where
        !blen = I# (tmo *# sizeOf# (undefined :: Double))
      ```

      Would be much nicer if this was generated, exported and only
      then used in the typclass. I understand that this is much more
      effort on `store` authors however so this one is more wishful
      thinking if anything.

   To conclude this point, please think of the users and expose
   non-trivial internal functions that may be useful to anyone. Stick
   them in `.Internal` with no guarantees, mark them INLINE if you're
   relying on it, just let us use them.

3. I'm still going to get access to internals if I want to. Please
   don't make me do the extra work.

   If I need the internals and they are not expose, here are my
   options.

   * Clone the package, hack the source to expose what I need, use it.
   * Ask author to do it. Wait and hope they do it and quick. In
      meantime, I'm probably forced to do a fork anyway.
   * Don't do anything but simply use existing API. Cry over
     performance you're losing while the code you need to go omgfast
     is _right there_.
   * If the function is small and self-contained, copy and paste all
     the code needed to make it work. Often not an option unless you
     want to copy whole package because likelyhood is that internals
     that you need are also not expose. This is basically a less
     drastic fork if you're lucky, a full fork if you're not. Downside
     is that it's now your code to live with.
   * You could also complain to author that the library API is not
     good enough and it should do the thing you want it to do.
     Sometimes you may even be right! More often than not, if you're
     looking for internals, it probably doesn't belong in the API to
     start with and redesigning the library to include your usecase is
     unreasonable/impossible/not cost effective.

   It's better for everyone if I can access internals. I'm happy
   because I get to do what I want. The author is happy because I'm
   not bothering him on the issue tracker and I'm talking about how
   fast and awesome and accessible the library is.

4. I want to use your library _right now_ but it's doing something
   poorly.

   This usually warrants a fork/pull request anyway but sometimes it
   really may be the case that a library is doing mostly what I want
   but some things are poorly designed or implemented. I could
   workaround this on the spot and send a pull request later but if
   you're not exposing internals then I'm out of luck. Either I am
   forced to fork it or use another library. If short on time and
   alternative exists, latter sounds like a better option even if the
   original library is much nicer in general.

### About Haddock for .Internal modules.

A side-point I want to mention is something that few packages like to
do that I really hate. Let's use `bytestring` as an example. Let's
look at the [main page for the
package](https://hackage.haskell.org/package/bytestring-0.10.8.2).
`.Internal` modules appear but have no documentation! Indeed often you
can't even tell that the package exposes internals. Sometimes these
modules are even documented but a Haddock pragma is used to hide them.

Please stop doing this. It's just a massive inconvenience. I have to
first look through the cabal file to check what modules are really
exposed. Then I have to source dive to see what functions are
available. I have to check which are actually exported. You as an
author lose the opportunity to plaster with massive letters at the top
of the module that this is an internal module with no guarantees.

If you don't want to document your .Internal module that's up to you
but at the very least don't make me look through source every time I
want to check something. "You're forced to looked at source so you
have to see what the functions are actually doing" is not a
good argument. I probably already did and you're not giving me any
guarantees for the module anyway so there's no need to make it a pain
to actually use it.


## Conclusion

Next time you release a library, please consider exposing internals
one way or another. You can keep your usual hidden module hierarchy if
you want, just make a kitchen-sink module that re-exports those.
Exposing typeclasses that nearly do what one might want but not quite
and not exposing the implementations also doesn't count. If your
implementation looks like

```haskell
instance C Foo where
  someFunc = bar . someFuncImpl
```
expose `someFuncImpl`.

Last remark is that I am not against hidden modules. I am against
using them to hide useful code. You can still use hidden modules for
organising your project in a way you find convenient but I would very
much appreciate it if you could expose all that useful code through
some means in the end.
