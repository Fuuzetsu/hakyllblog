---
date:   2014-01-06 01:17:13
title:  Fix your Hackage documentation
author: Fūzetsu
---

This is a friendly reminder to fix your Hackage documentation.

I've been feeling that over the last couple of months, I had to click
on an older version of the package far more than before. Considering
the Hackage 2 move around August, that was the first suspect. A quick
chat in `#hackage` and it seems I was not the only one with that
feeling, although no one could tell me what exactly was wrong.

Before I could went off on the mailing lists and asked for answers, I
needed some numbers. People like numbers. If you're not interested in
this post, I ask that you at least read
[this thread on cabal-devel][4]. Make sure you read the replies as I
show how to do some things you might find useful.

Last night a scraped documentation information for the most recent
version of all packages on Hackage. I do not have any more access to
Hackage than most mere mortals do so this was the only way.
[Hackage API][1] does exist but is very badly documented and it doesn't
seem to do JSON ever though it claims to do so and the [relevant GitHub
task][2] is on ex-TODO. I used HXT to parse the output pages. You can
see the hacky code [as a Gist][9] or [as a file][10].

So considering this is a reminder, I better provide some information.
You can find a list of package uploaded in 2013 and 2014 for which the
documentation was deemed broken [by clicking this][3]. There are
multiple reasons why a package could fail. To reiterate from the
e-mail I sent to the mailing lists, here are failures we can't do that
much about:

* Dependencies fail to build so your package does
* Your package fails to build directly
* Your package requires non-cabal libraries which aren't installed
* Your package requires different version of install libraries

There are however failures you can do something about straight away:
Haddock failures. If your package was listed [here][3] and had
‘InstallOk’ next to it, your Haddock comments are probably wrong. This
means you should view the build log and see what's wrong.

Viewing the Hackage build log
-----------------------------
Here's a very poorly but useful feature of Hackage: you can usually
view your build log. If on my list, you have a MissingDocs reason
that's not ‘Nothing’, you can view this log. If it's Nothing, sorry
but you're out of luck. See [this comment][5] on what is happening. I
will show you how to upload documentation manually later for these cases.

1. Identify your package version. My list shows this but you should
   also be able to just go on Hackage and see what's broken. As an
   example, I'll be using my tiny yi-monokai-0.1.1.1 package for which
   the build has failed.
2. Read the build log. You can get the build status for Hackage
   packages like so:
   <http://hackage.haskell.org/package/yi-monokai-0.1.1.1/reports/>.
   Considering your package got InstallOk, this will exist. You can go
   to the first build status log like so:
   <http://hackage.haskell.org/package/yi-monokai-0.1.1.1/reports/1> and you
   can get to the actual build log like this:
   <http://hackage.haskell.org/package/yi-monokai-0.1.1.1/reports/1/log>. The
   pattern should be easy to spot.

Somewhere at the bottom of the build log there should be a reason for
failure. If it's a Haddock error, fix it. I made your job easier and
made [a list of packages which seem to simply have malformed Haddock
comments][6]. I had manually fixed and formed pull request for the
bottom third of these. It's usually very easy to fix! See
[Haddock manual][12] for help on syntax. You can also ping me on IRC
(Fuuzetsu) and I'll be happy to point out what might be wrong.

Another kind of failure one might get this way is HsColour failure:
Hackage can run Haddock with `--hyperlink-source` and I saw an error
yesterday caused by bad file encoding and HsColour failing. This is
unusual (your files should be UTF-8 to begin with).

If all it is a Haddock parse error, simply fix it and upload a new
version of the package. Anyone browsing will be grateful!

If it's something else, read on.

Uploading documentation manually
--------------------------------
If your failure wasn't a simple Haddock markup problem, it might not
be plausible to have Hackage build your documentation. An easy example
is missing C libraries. While ideally Hackage should have means of
providing these, it currently doesn't.

If you don't have a build log for your package (‘Nothing’ on my list),
this is probably the only way to get documentation too.

Here I'll outline how to create and upload documentation by hand for
your package. The change should immediate once uploaded.

Before you do this, check your build log. If it's just a Haddock
failure, fix it and upload the package afresh. Don't upload
documentation which differs from the actual package version.

You first need to make your documentation. I'll be using
‘yi-monokai-0.1.1.1’ package as an example.

1. Navigate to your project's directory:

     `cd ~/programming/yi-monokai`

2. Build package with documentation:

     `cabal configure && cabal build && cabal haddock
     --hyperlink-source`

3. Navigate to where your documentation was generated.

     `cd dist/doc`

4. Rename your docs directory to a format Hackage expects it in. It is
   ‘packagename-version-docs’.

     `mv yi-monokai yi-monokai-0.1.1.1-docs`

5. Create an archive of your directory. It has to be in a specific
   format and you'll need the `--format=ustar` flag.

     `tar -c -v -z -Hustar -f yi-monokai-0.1.1.1-docs.tar.gz
     yi-monokai-0.1.1.1-docs`

6. Upload the docs to Hackage. You need to make a POST to a specific
   URL. Triple check your package version: you don't want to be
   uploading documentation for the wrong thing. The username and
   password are your Hackage credentials.

     `curl -X PUT -H 'Content-Type: application/x-tar' -H
     'Content-Encoding: gzip' --data-binary
     '@yi-monokai-0.1.1.1-docs.tar.gz'
     'http://USERNAME:PASSWORD@hackage.haskell.org/package/yi-monokai-0.1.1.1/docs'`

7. Go on Hackage and see whether it worked. Your docs should come up
   straight away. If they haven't, worry. Check your URL. Make sure
   you didn't get any error messages from Hackage when uploading the
   package.

Here's a quick Bash script to automate it a bit: [as a Gist][7] or
[as a file][8]. Perhaps a library to interface with Hackage needs to
be written. In fact, that'd be pretty useful. Put it somewhere in your
PATH, go into your project's directory and use it like this:

`hackagedocs yi-monokai 0.1.1.1 username password`

It's is very naive, I am in fact writing it while I'm writing this
blog post. Please adjust the commands inside as you see necessary.

Disclaimer: I don't know Bash, I'm just making stuff up from snippets
of scripts I have lying around.

I hope I encouraged you to make your Hackage documentation work again!
Have [some fan service][11] for making it all the way to the end of
the tedious post.

[1]: http://hackage.haskell.org/api
[2]:
https://github.com/haskell/hackage-server/issues/11#issuecomment-31593476
[3]: http://fuuzetsu.co.uk/misc/sorted.txt
[4]: http://www.haskell.org/pipermail/cabal-devel/2014-January/009629.html
[5]:
https://github.com/haskell/hackage-server/issues/145#issuecomment-30129142
[6]: http://fuuzetsu.co.uk/misc/faileddocs.txt
[7]: https://gist.github.com/Fuuzetsu/8276421
[8]: http://fuuzetsu.co.uk/misc/hackagedocs
[9]: https://gist.github.com/Fuuzetsu/8276445
[10]: http://fuuzetsu.co.uk/misc/H.hs
[11]: http://fuuzetsu.co.uk/images/shana_meronpan.jpeg
[12]: http://www.haskell.org/haddock/doc/html/
