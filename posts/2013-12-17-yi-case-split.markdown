---
date:   2013-12-17 10:04:08
title:  Yi case split
author: Fūzetsu
---

I've been hacking on the [Yi](https://github.com/yi-editor/yi) text
editor a bit recently. I'm an emacs user day-to-day, hour-to-hour (see
my [emacs.d](https://github.com/Fuuzetsu/.emacs.d)) but as a
Haskeller, ELisp nothing short of disgusts me and its age doesn't
really help. Ideally I'd like to write extensions in Haskell and it
just happens that Yi lets you do that: you get to have proper
dependency management through Cabal and you can use all the Haskell
libs that you can think of.

I'm also a rather big fan of Agda's case-split. If you don't know what
it is, you're missing out. It allows you to point your editor at a
variable and have it split on the structure base on its type.

For a quick Haskell example:

```haskell
data Foo a b = FooA a | FooB b

someFunc :: Foo a b -> Either a b
someFunc x = ?

```

Note that I'm using ‘?’ to denote a hole. In fact, in GHC 7.8 we're
getting a similar concept, TypeHoles, denoted with an underscore.

In any case, in Agda we can simply point emacs at ‘x’ and ask it to
split. Suddenly, almost like magic, we get

```haskell
data Foo a b = FooA a | FooB b

someFunc :: Foo a b -> Either a b
someFunc (FooA x) = ?
someFunc (FooB x) = ?

```
(In fact in Agda we can go step further and ask Agda to solve it all
for us which would fill in the reasonable definition in place of the holes)


This is extremely convenient and something I always wanted in Haskell.
While we can't reach the same level of underlying magic in Haskell (we
don't know as much about the types and Agda can do great things like
prove patterns impossible on the fly), we can try to imitate. So when
today I finally sat down to implement it in my emacs, I decided to do
it in Haskell and use it in Yi instead. Below is a very preliminary
result of a night of sweating and swearing:

![case-split](/images/case-split-s.gif)

Apologies for the poor quality, I struggled to record this.

Why am I posting about this anyway? Is it because I'm hoping to get
some attention to Yi? Certainly! It's mostly undocumented with age-old
code and archaic module structure but this is precisely why there's a
lot of low-hanging fruit.

```
<maybefbi> Fuuzetsu: Gotta install yi I guess
```

Oh, I suppose it worked!

It's certainly possible to get Yi to the point where it's very usable
or even better than your current editor. The downside is that the
wealth of existing modes for other editors is not available but the
huge upside is that they can now be written in Haskell.


The case-split code is not on Hackage yet, it won't be for another few
days. If you're _really_ eager to try it, you can find it in
[this GitHub project](https://github.com/Fuuzetsu/yi-haskell-utils)
but I highly advise against this.

The colour scheme used (safe for the weird blue overlay which comes
from somewhere in the conversion to .gif) is a port of monokai colour
scheme, available
[on Hackage](http://hackage.haskell.org/package/yi-monokai) or
[on my GitHub](https://github.com/Fuuzetsu/yi-monokai). You can see
the usage in [my Yi config](https://github.com/yi-editor/yi).


PS: While it might look great, it has only been a night of fighting
with Cabal packages, ghc-mod and lack of documentation so it is very
fragile and the code is disgusting. For example, it will blindly
insert variables so it's perfectly possible to end up with name
clashes. It will also try to split on anything it can get a type of,
no matter where in the code it is and such. There are many problems
but I think they aren't very difficult to address. Perhaps tomorrow it
will work much better.


PPS: If you want to see some magical features, use Agda-mode in emacs
for a bit.
