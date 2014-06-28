---
date:   2014-06-28 08:14:30
title:  My experience with NixOS
author: Fūzetsu
---

This post is for Haskellers interested in nix (the package manager)
and maybe even NixOS (a distribution built on nix). If you're not
interested then skip it, but I know many people are. It describes how
I made the switch and some of my experiences since. I have put off
this blog post for a long time, hoping to write it up once I have
everything working just like I want it but I was finally motivated to
write it up by people expressing interest on IRC. I know many people
want to switch but aren't quite there yet, hopefully this can help
them make the decision. If you're interested in nix but not NixOS, you
probably want to just skim the beginning.

Please note that things contained here are just my opinions and I'm
not some NixOS guru so things stated here may well be inaccurate.

A couple of weeks ago I have switched to NixOS. Like many, I have seen
[the blogpost by ocharles][ocharles] and have since thought ‘It'd be
great to switch but I'd hate to put in the effort’ but the thought
crept in. I ask that you read that blog post first. I have even
started to set up NixOS on a separate hard-drive. Recently I have
finally decided to retire my trusty ThinkPad X61s on which I did my
hacking for the past three years: it was overheating, had holes
through it (don't ask), falling apart and I have took it apart so many
times that it's a miracle it even stayed together. This was a perfect
chance. I have taken out the SSD (which cost me more than the netbook
itself) and repurposed one of my fileservers which was running Gentoo
into a desktop machine.

Probably the most vital resource when making the switch is the NixOS
is the [NixOS manual][nixosmanual]. I'll not go over the installation
process but you can find my configuration file [here][nixosconfig].

My current set-up is XMonad without a DE, using SLiM as a log-in DM.

At the beginning I struggled. I had problems understanding how things
worked and some software I wanted to use was simply not packaged. I
spent the first couple of weeks with KDE and without some software I
wanted. This is a bit of a downside: the number of packages is not the
greatest of all distributions. Please don't get me wrong, there *is* a
lot of software already but the chances are that if you're using
something not that common, you might have to package it yourself. The
upside is, it's easy to do.

I will briefly describe some things which will related to Haskell
development later. There is a thing called [Hydra][hydra], it's a build-bot
that NixOS uses. There is a thing called [nixpkgs][nixpkgs], it is a
repository of packages used by NixOS and also nix itself if you aren't
going for the full OS. nixpkgs is essentially a big repository of nix
expressions. Hydra looks at this and builds the expressions, resulting
in packages. Main re-distribution works in channels: a user subscribes
to a channel and when we ask for some package to be installed, this is
where the information is taken from. Official channels are effectively
nixpkgs at some commit: nixos channel might be a few weeks behind
nixpkgs HEAD, nixos-unstable is usually a few days. Channels are
updated when Hydra finishes to build a particular jobset: this means
you get binaries for the default settings of all Hydra-built packages.
This includes Haskell packages!

I will now describe how I have been doing Haskell development. Again,
note that this is constantly evolving while I discover new things.

Haskell development with nix/NixOS
==================================

Firstly, NixOS is not necessary to benefit. Pretty much everything I
say here is due to nix itself.

Perhaps the main motivation for using nix is wanting to avoid cabal
hell. The presence of cabal sandboxes and freezing of dependencies has
allowed many people to avoid the problem. I myself used sandboxes very
soon after they came out and use cabal-dev before that. My main
problem with sandboxes is managing them: are you sandboxing a new
project? Come back in an hour when text, hxt, lens, attoparsec,
haskell-src-exts and whatever else you happen to be using have
compiled for the 50th time on your machine. Sure, one can use shared
sandboxes but it is a *massive* pain. I have wasted hours of my life
recompiling same dependencies. nix allows you to avoid this.

I will consider a few scenarios and any potential problems that might
come up and how I have dealt (or not dealt!) with them so far.

You have your project. Perhaps the first thing you do is write the
cabal file or maybe you already have one but you want to use nix. When
we develop, we often want to actually be able to be in the environment
of the package, be able to run ghci and all that jazz. There's a tool
called `nix-shell` which can help you. This effectivelly allows you
drop into a sandbox of your project. This is the magical thing
[ocharles refered to in his blog post][ocharles]. What he did not
mention is that you can generate on of these expressions necessary to
use `nix-shell`. Here's a real example:

```
[shana@lenalee:/tmp]$ cat Yukari.cabal
name:                Yukari
version:             0.1.0.0
synopsis:            Command line program that allows for automation of various tasks on the AnimeBytes private tracker website.
homepage:            http://github.com/Fuuzetsu/yukari
license:             GPL-3

license-file:        LICENSE

author:              Mateusz Kowalczyk
maintainer:          fuuzetsu@fuuzetsu.co.uk
category:            Utils
build-type:          Simple
cabal-version:       >=1.8

executable yukari
  main-is:             src/Main.hs
  build-depends:       base ==4.*, Yukari

library
  default-language:     Haskell2010

  build-depends:       base ==4.*, curl ==1.3.*, HTTP ==4000.*, filepath ==1.3.*
                       , directory ==1.2.*, bytestring ==0.10.*, network ==2.5.*
                       , text ==1.1.1.*, attoparsec ==0.12.*, HandsomeSoup ==0.3.*
                       , hxt ==9.*, download-curl ==0.1.*, dyre

  hs-source-dirs:       src
  exposed-modules:
    Utils.Yukari
    Utils.Yukari.Crawler
    Utils.Yukari.Filters
    Utils.Yukari.Formatter
    Utils.Yukari.Parser
    Utils.Yukari.Settings
    Utils.Yukari.Spender
    Utils.Yukari.Types

test-suite spec
  type:             exitcode-stdio-1.0
  default-language: Haskell2010
  main-is:          Spec.hs
  hs-source-dirs:
      test

  build-depends:       base ==4.*, Yukari, hspec, QuickCheck == 2.*,
                       filepath==1.3.*, directory ==1.2.*
```

Then with little help of cabal2nix (the dummy sha256 parameter is a
hack here as we're generating an expression for a source repository).:

```
[shana@lenalee:/tmp]$ cabal2nix Yukari.cabal --sha256 foo
{ cabal, attoparsec, curl, downloadCurl, dyre, filepath
, HandsomeSoup, hspec, HTTP, hxt, network, QuickCheck, text
}:

cabal.mkDerivation (self: {
  pname = "Yukari";
  version = "0.1.0.0";
  sha256 = "foo";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    attoparsec curl downloadCurl dyre filepath HandsomeSoup HTTP hxt
    network text
  ];
  testDepends = [ filepath hspec QuickCheck ];
  meta = {
    homepage = "http://github.com/Fuuzetsu/yukari";
    description = "Command line program that allows for automation of various tasks on the AnimeBytes private tracker website";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
```

Note that `cabal2nix` generates expressions suitable for nixpkgs. To
use it for a shell environment, I ammend the resulting expression into
following:

```
{ pkgs ? (import <nixpkgs> {})
, haskellPackages ? pkgs.haskellPackages_ghc763
}:

haskellPackages.cabal.mkDerivation (self: {
  pname = "Yukari";
  version = "0.1.0.0";
  src = /home/shana/programming/yukari;
  isLibrary = true;
  isExecutable = true;
  buildDepends = with haskellPackages; [
    attoparsec curl downloadCurl dyre filepath HandsomeSoup HTTP hxt
    network text
  ];
  testDepends = with haskellPackages; [ filepath hspec QuickCheck ];
  meta = {
    homepage = "http://github.com/Fuuzetsu/yukari";
    description = "Command line program that allows for automation of various tasks on the AnimeBytes private tracker website";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
```

If at any point I want to use a different compiler version, I only
have to change it at the top (or use a flag to nix-shell)  and it will
automagically all just work. Now I can use this sandbox:

```
[shana@lenalee:~/programming/yukari]$ nix-shell --pure

[nix-shell:~/programming/yukari]$ cat .ghci
:set -isrc -fbreak-on-error
[nix-shell:~/programming/yukari]$ ghci
GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
package flags have changed, resetting and loading new packages...
Loading package array-0.4.0.1 ... linking ... done.
Loading package deepseq-1.3.0.1 ... linking ... done.
Loading package containers-0.5.0.0 ... linking ... done.
Loading package filepath-1.3.0.1 ... linking ... done.
Loading package old-locale-1.0.0.5 ... linking ... done.
Loading package time-1.4.0.1 ... linking ... done.
Loading package bytestring-0.10.0.2 ... linking ... done.
Loading package unix-2.6.0.1 ... linking ... done.
Loading package directory-1.2.0.1 ... linking ... done.
Loading package old-time-1.1.0.1 ... linking ... done.
Loading package pretty-1.1.1.0 ... linking ... done.
Loading package process-1.1.0.2 ... linking ... done.
Loading package Cabal-1.16.0 ... linking ... done.
Loading package binary-0.5.1.1 ... linking ... done.
Loading package bin-package-db-0.0.0.0 ... linking ... done.
Loading package hoopl-3.9.0.0 ... linking ... done.
Loading package hpc-0.6.0.0 ... linking ... done.
Loading package template-haskell ... linking ... done.
Loading package ghc-7.6.3 ... linking ... done.
Prelude> :l  Utils.Yukari
[1 of 7] Compiling Utils.Yukari.Types ( src/Utils/Yukari/Types.hs, interpreted )
[2 of 7] Compiling Utils.Yukari.Settings ( src/Utils/Yukari/Settings.hs, interpreted )
[3 of 7] Compiling Utils.Yukari.Parser ( src/Utils/Yukari/Parser.hs, interpreted )
[4 of 7] Compiling Utils.Yukari.Formatter ( src/Utils/Yukari/Formatter.hs, interpreted )
[5 of 7] Compiling Utils.Yukari.Crawler ( src/Utils/Yukari/Crawler.hs, interpreted )
[6 of 7] Compiling Utils.Yukari.Spender ( src/Utils/Yukari/Spender.hs, interpreted )
[7 of 7] Compiling Utils.Yukari     ( src/Utils/Yukari.hs, interpreted )
Ok, modules loaded: Utils.Yukari, Utils.Yukari.Crawler, Utils.Yukari.Formatter, Utils.Yukari.Settings, Utils.Yukari.Spender, Utils.Yukari.Parser, Utils.Yukari.Types.
```

The `--pure` stops any ‘globally’ installed tools or packages from
polluting the environment which ensures that we only use what we say
we do: no surprises because other developer didn't have
‘somespecialprogram’ installed! Personally I currently use emacs with
haskell-mode and I want a REPL in emacs. nix-shell lets you do this.
The way I do it is to eval `(setq haskell-program-name "nix-repl --pure
--command "ghci")`.

So we managed to sandbox a single project. Cool, but what about if we
want to depend on another project? It's often the case that our
project depends on another of our projects which might not be on
Hackage or we want to work against dev version or ….

I do this with Haddock: we recently split out haddock parser into a
sub-library, ‘haddock-library’. I simply wrote an expression for
haddock-library and then import it from haddock expression. Simple:

```
[shana@lenalee:~/programming/haddock]$ cat default.nix
{ haskellPackages ? (import <nixpkgs> {}).myHaskellPackages_ghcHEAD
, haddockLibrary ? (import /home/shana/programming/haddock/haddock-library
    { haskellPackages = haskellPackages; })
}:

haskellPackages.cabal.mkDerivation (self: {
  pname = "haddock";
  version = "2.15.0";
  src = /home/shana/programming/haddock;
  buildDepends = with haskellPackages;
                   [ Cabal deepseq filepath ghcPaths xhtml haddockLibrary ];
  testDepends = with haskellPackages; [ Cabal deepseq filepath hspec QuickCheck ];
  isLibrary = true;
  isExecutable = true;
  enableSplitObjs = false;
  noHaddock = true;
  doCheck = true;
})
[shana@lenalee:~/programming/haddock]$ cat haddock-library/default.nix
{ haskellPackages ? (import <nixpkgs> {}).myHaskellPackages_ghc763
}:
let
  inherit (haskellPackages) cabal deepseq QuickCheck hspec baseCompat;
in
cabal.mkDerivation (self: {
  pname = "haddock-library";
  version = "1.1.0";
  src = /home/shana/programming/haddock/haddock-library;
  testDepends = [ QuickCheck hspec baseCompat ];
  buildDepends = [ deepseq ];
  isLibrary = true;
  isExecutable = false;
  enableSplitObjs = false;
  doCheck = true;
})
```

There are a couple of things going on here. Firstly, you can see that
haddock-library by default uses GHC 7.6.3: `haskellPackages ? (import
<nixpkgs> {}).myHaskellPackages_ghc763`. This is fine but when I'm
working with Haddock itself, I want to make sure this gets built with
same version as haddock, so I have

```
, haddockLibrary ? (import /home/shana/programming/haddock/haddock-library
    { haskellPackages = haskellPackages; })
```

This makes sure we use the same set of packages in both so when
`haddock` uses GHC HEAD then so does `haddock-library`. To nix
enthusiasts out there, I'm aware I can use ‘inhert’, just didn't get
around to it.

Now whenever I change things under haddock-library and drop into
haddock shell, it will automagically get rebuilt.

Better yet, I do this with GHC itself! If you'll notice, I'm importing
`(import <nixpkgs> {}).myHaskellPackages_ghcHEAD`. If you look in my
[nixpkgs config][nixpkgsconfig] you'll find some incantations of
following nature:

```
{ pkgs }:

{ packageOverrides = self: with pkgs; rec {

  haskellPackages_ghcHEAD = self.haskell.packages {
    ghcPath = /home/shana/programming/ghc;
    ghcBinary = self.haskellPackages.ghcPlain;
    prefFun = self.haskell.ghcHEADPrefs;
  };

  …
```

What's going on here? Well, a few things. First I'm overwriting a
thing called `ghcPath` to `/home/shana/programming/ghc`. This points
to my local GHC HEAD checkout. In there I have another nix expression
which describes how to build GHC HEAD. This means that yes, I am able
to have Haddock depend on a checkout of GHC itself. Here is that GHC
expression in full:

```
{ pkgs ? (import <nixpkgs> {})
, stdenv ? pkgs.stdenv
, ghc ? pkgs.ghc.ghc782
, perl ? pkgs.perl
, gmp ? pkgs.gmp
, ncurses ? pkgs.ncurses
, happy ? pkgs.haskellPackages.happy
, alex ? pkgs.haskellPackages.alex
, automake ? pkgs.automake
, autoconf ? pkgs.autoconf
, git ? pkgs.git
, libxslt ? pkgs.libxslt
, libxml2 ? pkgs.libxml2
, python ? pkgs.python
}:

stdenv.mkDerivation rec {
  name = "ghc-${version}";
  version = "7.9.20140624";

  src = "/home/shana/programming/ghc";

  buildInputs = [ ghc perl gmp ncurses automake autoconf
                  git happy alex libxslt libxml2 python ];

  enableParallelBuilding = true;

  buildMK = ''
    libraries/integer-gmp_CONFIGURE_OPTS += --configure-option=--with-gmp-libraries="${gmp}/lib"
    libraries/integer-gmp_CONFIGURE_OPTS += --configure-option=--with-gmp-includes="${gmp}/include"
    DYNAMIC_BY_DEFAULT = NO
    BuildFlavour = quick
  '';

  preConfigure = ''
    echo "${buildMK}" > mk/build.mk
    perl boot
    sed -i -e 's|-isysroot /Developer/SDKs/MacOSX10.5.sdk||' configure
  '' + stdenv.lib.optionalString (!stdenv.isDarwin) ''
    export NIX_LDFLAGS="$NIX_LDFLAGS -rpath $out/lib/ghc-${version}"
  '';

  configureFlags = "--with-gcc=${stdenv.gcc}/bin/gcc";

  # required, because otherwise all symbols from HSffi.o are stripped, and
  # that in turn causes GHCi to abort
  stripDebugFlags = [ "-S" "--keep-file-symbols" ];

  meta = {
    homepage = "http://haskell.org/ghc";
    description = "The Glasgow Haskell Compiler";
    maintainers = [
      stdenv.lib.maintainers.marcweber
      stdenv.lib.maintainers.andres
      stdenv.lib.maintainers.simons
    ];
    inherit (ghc.meta) license platforms;
  };
```

You don't have to be able to understand this but know that whenever I
want to update my GHC HEAD, all I have to do is to update the
repository (through usual sync-all GHC script) and then bump up the
version in above expression. Now if I go to drop into a `nix-shell`
for Haddock, it will notice the change and build GHC HEAD.

Now to explain another bit of my config:

```
  myHaskellPackages_ghcHEAD = pkgs.recurseIntoAttrs (haskellPackages_ghcHEAD.override {
    extension = se : su : {
      syb = se.callPackage /home/shana/programming/nixpkgs/pkgs/development/libraries/haskell/syb/0.4.2.nix {};
      vty_5_1_0 = se.callPackage /home/shana/programming/nix-project-defaults/vty/5.1.0.nix {};
      mtl = se.callPackage /home/shana/programming/nix-project-defaults/mtl/2.2.1.nix {};
      testFrameworkSmallcheck =
        se.callPackage /home/shana/programming/nix-project-defaults/test-framework-smallcheck {};
    };
  });

  …
}; }
```

What I'm doing here is defining or overwriting packages in the Haskell
package set: as you can see, I'm defining vty_5_1_0 and setting mtl
default to 2.2.1. Why? They were either not at that moment in my
version of nixpkgs (my channel hasn't caught up) or I wanted to use
different defaults. It's as easy as the above. This brings me to the
next point.

What happens when nixpkgs doesn't have something you need?

1. Create an expression for it. This is as easy as using cabal2nix. If
   it's on hackage, it's even easier:

    ```
    [shana@lenalee:~/programming/haddock]$ cabal2nix cabal://text
    { cabal, deepseq, HUnit, QuickCheck, random, testFramework
    , testFrameworkHunit, testFrameworkQuickcheck2
    }:

    cabal.mkDerivation (self: {
      pname = "text";
      version = "1.1.1.3";
      sha256 = "1yrzg449nbbzh2fb9mdmf2jjfhk2g87kr9m2ibssbsqx53p98z0c";
      buildDepends = [ deepseq ];
      testDepends = [
        deepseq HUnit QuickCheck random testFramework testFrameworkHunit
        testFrameworkQuickcheck2
      ];
      meta = {
        homepage = "https://github.com/bos/text";
        description = "An efficient packed Unicode text type";
        license = self.stdenv.lib.licenses.bsd3;
        platforms = self.ghc.meta.platforms;
      };
    })
    ```

2. Point to it somehow from your project. Two main ways are to either
   add it to your package base (as seen in my config snippet) or do it
   directly from a project (as seen from my haddock expression
   snippet).

3. Make a pull request to [nixpkgs][nixpkgs] so everyone can benefit.
   Please read [contribution][nixpkgscontrib] NixOS wiki page on how
   to contribute.


So is this better than cabal sandbox? In my opinion, yes, here's why I
think so:

* Automatically share binary results: are you working with dev version
  of a library? After you build it once, all your other projects
  benefit: nix will not rebuild a dependency ‘just because’, it will
  re-use the binary across all your projects that say they want it!
  This is already much better than sandboxes where you have to
  explicitly share.

* You can specify more than Haskell packages: cabal only allows you to
  specify Haskell dependencies but what if you require gcc too? Maybe
  you have development tools like ghc-mod that you want to use. When I
  wanted to use ghc-mod across projects with multiple GHC versions it
  was absolute nightmare. nix will let you do this effortlessly be it
  with Haskell packages or external tools or even Haskell tools which
  depend on specific versions of GHC. Remember, we can sandbox GHC
  versions and the tools depending on them.

* It's not limited to Haskell software. You can sandbox just about
  anything you can imagine. You absolutely have to run some PHP
  script? Sure, if it's a bit complicated then write a nix expression
  for it and run. If it's simple, `nix-shell -p php` will drop you in
  a shell with PHP available, automatically pulling in all
  dependencies. Once you're done with that environment, no longer
  dependencies will be removed during garbage collection.

* Uses binaries whenever available while cabal sandbox will usually
  leave you waiting for everything to compile.

Even `hakyll` which is a Haskell program that will generate a page
from this Markdown post is going to be used by `nix-shell -p
haskellPackages_ghc763.ghc -p haskellPackages_ghc763.hakyll --pure`: I
don't need it day to day so I'll just let it get garbage collected at
next opportunity.

The downsides of using nix-shell for Haskell projects:

* It's a less-documented process. For more complicated setups, it
  might take a bit of figuring out how to get it to work. An example
  is me trying to figure out how to get Yi to see its own available
  libraries at runtime which is required for dynamic reloading &c.

* The workflow is a bit different from what you might be used to.
  Currently I'm using `eval "$configurePhase" && eval "$buildPhase"`
  in my projects which behind the scenes runs cabal. Note that there
  are people who use nix and stick with their usual development
  workflow of using cabal configure/build themselves so it is

* Rarely it might be necessary to run cabal by hand if your project
  requires it. My use-case was generating symbols that we get from
  cabal such as those used by CPP library version pragmas. This is not
  too common however.

* There are two places to update when you add/remove dependencies to
  the project: nix expression and cabal file. I consider this very
  minor considering it's probably a word change in each. To be clear,
  your cabal projects keep their cabal files, using nix does not
  mean that yourproject.cabal is no longer used.


Summary
========

I'll give a breakdown of what I like and dislike about nix and NixOS
so far.

What I like:

* NixOS configuration is a pleasure. You no longer have to run around
  all over your system in hunt of configuration files, you now have a
  config file that you yourself decide how to split up (if at all) and
  if you screw anything up, you can always roll back.

* Packaging software is fairly easy. There *are* things that are
  difficult to package but in huge majority of cases, it is a few
  lines. It's not terribly difficult to get started.

* Binaries for Haskell packages that aren't terribly out of date. Many
  binary distros out there have outdated Haskell packages if at all.
  Here there are tools to generate expressions from cabal files so
  updating is not a chore. If all you're doing is a version bump then
  it's as easy as changing a line or two and making a pull request.
  Hydra is nearly always churning through new Haskell packages to make
  sure it's all up to date with change dependencies.

* I'm not losing sleep over possibility of cabal hell.

* I'm not losing days of my life staring at lens or text build.

* Switching between GHC versions is trivial. In the past I was
  switching symlinks between GHC versions and carefully sandboxing
  everything. While it worked for development, it certainly did not
  work for anything using currently-active package databases (ghc-mod
  anyone?).

* I don't have to think about things clashing. If one project wants
  text-0.11, another -1.0 and another -dev999999Ijustmadeachange
  then there's no real hassle on my part.

* Easy deployment. If you're a company, you can set up Hydra to build
  your software for you. If you're a sysadmin, you can install nix and
  your users are able to install they software they want without
  bothering you: nix allows regular users to install software into
  their profile.

* You can roll-back through upgrades whether it be system upgrades or
  user profile upgrades. Every time you run `nix-env -i` to install a
  package, a new generation is created so you can roll-back later if
  you need to.

What I dislike:

* The documentation is a bit scarce. I end up having to look through
  package or NixOS module sources more than I'd like to.

* The nix expression language is not statically typed yet and error
  messages are often complete ass.

* On more popular distros, often one can use search engines to find
  people who already had the problem. On NixOS such information
  sometimes just does not exist. I have been relentlessly posting to
  the nix mailing list to hopefully change this a bit and to actually
  find out what I wanted.

* One has to either disallow unfree packages completely or allow them.
  It's not possible to say that we're OK with pulling in unfree nvidia
  drivers but other than that we want nothing unfree on our system.

* I'm used to being able to customise each software package in 50
  different ways. Often in nixpkgs the package maintainers don't take
  time to expose various options. To follow up, Hydra only builds
  packages with the default flags. The current hack is to define
  multiple packages with different default flags.

* Pull requests in certain areas can take a longer time and/or some
  reminding before getting merged. Haskell-related PRs get merged
  quickly however.

* The package managment tools are not as up to scratch as they are on
  older distributions. Gentoo has some great tooling. I put it towards
  young age of the distribution.

* Getting fixes from nixpkgs newer than your channel is a bit of a
  pain. You either check out the appropriate commit and apply patches
  on top or rebuild half of your system. I used to run against HEAD
  version of nixpkgs and found myself compiling a lot of stuff because
  Hydra didn't build it yet. I recommend nixos-unstable channel which
  is usually not far behind HEAD.

* systemd

* There's a policy to only keep latest versions of software around
  unless it's necessary to have more. This means that when you
  generate a nix expression from cabal file, it will try to use the
  defaults in nixpkgs rather than specific versions. While I dislike
  this quite a bit, there are a few things that can be done to keep
  things sane:

    * When you really need an older version, you can explicitly refer
      to it if in nixpkgs or refer to your local expression if it
      isn't in nixpkgs

    * If the package works with a newer version and it's just the case
      of a bump in cabal file, you can set ‘jailbreak = true’ which
      ignores what cabal says about versions.

    * Many Haskell packages already have multiple versions available
      so I find that in practice it is not a huge worry anyway. I
      initially feared (and still do a bit) a horrible version mess
      but it seems to well enough.

* There are no binaries yet for 7.8.2 of package versions which means
  if you use those, you'll have to wait a bit while they build just
  like you would have to with cabal install anyway. This is only
  temporary but might be a slight annoyance if you're expecting
  binaries for those. I think building binaries for 7.8 will be
  switched on with 7.8.3 out but this is speculation.

* It can be a bit disk-space heavy because we potentially hold onto
  many versions of the same package, just built with different
  dependencies. The two ways to save space are: optimise your store
  which uses hardlinks for identical files to save space (saves GBs)
  and garbage-collect which removes software that is no longer
  dependended on. Even after you say that you no longer want some
  software with `nix-env -e`, it stays on your system until
  garbage-collected.

It may look like there are many dislikes but they are mostly
annoyances or my incompetence. I definitely would recommend nix or
even NixOS if you are already considering a switch. I have to say that
I can not recommend switching to NixOS if you need your machine in
development-ready mode next morning because it can take a few days to
get everything going just the way you need it. I don't have this worry
with nix itself however which you can install alongside your distro.
If you're a working man, I believe you could set up NixOS in a VM
first and then simply carry over the config when you have everything
ready.

In general I find that I never worry whether my package database will
screw up or anything like that.

If you're interested, please swing by #nixos on Freenode. This and the
mailing list is the majority of my help has been coming from.

It's a bit of a hectic post so please feel free to
[contact me][contact] if you have questions and I'll try to answer to
best of my knowledge. Note that I'll almost certainly not see and/or
not reply to questions on reddit if this is to find its way there,
sorry.

[ocharles]: http://ocharles.org.uk/blog/posts/2014-02-04-how-i-develop-with-nixos.html
[nixosmanual]: http://nixos.org/nixos/manual/
[nixosconfig]: https://github.com/Fuuzetsu/nix-project-defaults/blob/master/nixos-config/configuration.nix
[hydra]: https://nixos.org/hydra/
[nixpkgs]: https://github.com/NixOS/nixpkgs
[nixpkgsconfig]: https://github.com/Fuuzetsu/nix-project-defaults/blob/master/nixpkgs-config/config.nix
[nixpkgscontrib]: https://nixos.org/wiki/Contributing
[contact]: /contact.html
