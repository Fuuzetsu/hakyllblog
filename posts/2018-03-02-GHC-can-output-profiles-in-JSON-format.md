---
title: GHC can output profiling information as JSON and you should use it.
author: Fūzetsu
---

This is just a small PSA. As of GHC 8.2, when [GHC is able to accept
`-pj` RTS
flag](http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#json-profile-format)
which will output things in a nice machine readable format. So, use
it. I'd even be inclined to say that you should _always_ use it and if
you want the old format, we can have a tool that can produce that.

Yesterday I released two packages with relation to this. First is just
a very simple and naive package that reads the output:
[ghc-prof-aeson](https://hackage.haskell.org/package/ghc-prof-aeson).
It's just about what you'd expect, type definition then `aeson`
deriving. I have thrown in Linux, OSX and Windows CI as a bonus.

The second package is
[ghc-prof-aeson-flamegraph](https://hackage.haskell.org/package/ghc-prof-aeson-flamegraph).
This one is similar to
[ghc-prof-flamegraph](https://hackage.haskell.org/package/ghc-prof-flamegraph)
except that it doesn't bundle FlameGraph tool and that I didn't have
to try to impose additional dependencies on the existing package. It
was also not completely straight-forward to integrate with it.
Primarily this package is motivated by [this long-standing
issue](https://github.com/fpco/ghc-prof-flamegraph/issues/10) where
parsing the profile would just fail.

The `-p` and `-P` &c. profiling output format is not reliable. If
you're doing retainer profiling, you now have all this extra stuff in
your profile. Just use `-pj` and save everyone's sanity.

Demo
-----

Running on a random profile I had lying around. Generated with `+RTS -pj`.

```
[shana@lenalee:~/programming/ghc-prof-aeson-flamegraph]$ nix run nixpkgs.stack nixpkgs.perl.FlameGraph
[shana@lenalee:~/programming/ghc-prof-aeson-flamegraph]$ stack exec --no-nix-pure -- bash -c 'cat /tmp/secret.prof | ghc-prof-aeson-flamegraph | flamegraph.pl > /tmp/secret.svg'
```

Censored to protect the innocent.

![censored_prof](images/ghc-prof-aeson-secret.png)


Conclusion
------------

You should start using `-pj` if you're using GHC profiling. Better,
you should (re)write tools to work with this format instead of the
awful ad-hoc parsing that existing tools do.
