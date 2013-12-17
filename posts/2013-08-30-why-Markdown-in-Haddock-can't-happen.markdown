---
date:   2013-08-30 02:15:52
title:  Why Markdown in Haddock will not happen
author: Fūzetsu
---

Today I'm going to talk about something that might disappoint a fair
amount of people in the community. Namely, why Markdown syntax in
Haddock is actually A Bad Idea™ and why it's not going to happen.

Note that the following was written about a week ago, when I sat down
to actually implement this so it might not be the most coherent thing
ever: it was just meant to serve me as a TODO list (in fact, this was
originally written in org-mode) as I go along. It is a rather long
post so as I sometimes do, I include a section dedicated to
summarising it all. If you are one of the proponents of an alternate
syntax for Haddock, I'd appreciate it if you read the reasoning for
this post before sending any angry e-mails!

Perhaps the most humorous part of this post is that it itself is
written in Markdown and then converted into HTML…


Background
==========
There has been a rather large push for Markdown to be allowed as a
language for Haddock comments. In fact, there were many more
propositions such as reST or even WikiCreole. The motivation is
that people would not have to learn a new markup syntax in order
to write documentation. I will present bits of what consists of
Markdown as per the [original Markdown documentation](http://daringfireball.net/projects/markdown/syntax). Note that
problems with Markdown have been pointed out: there is a whole
plethora of Markdown flavours out there, mostly stemming from the
fact that no formal specification actually exists. The differences
between some common formats can be tried out using [babelmark2](http://johnmacfarlane.net/babelmark2/).
This is rather unfortunate because while multiple people want to
see Markdown syntax available, they almost certainly want to see
the Markdown syntax and rules they are used to. It might look like
10 people uniformly want Markdown but it's actually the case that
2 people want GitHub Markdown, 2 people want Pandoc Markdown, 2
people want original Markdown and so on and so on. This means that
the Markdown flavour chosen would have to be rather agnostic of
any special features any of these might provide.

Furthermore, it should be noted that these suggestions were coming
with respect to Haddock as it was 2-5 months ago (Feb-June 2013)
which had many quirks and was a bit less expressive in terms of
nested markup and such. This means that such propositions could
have been motivated by:

* people know that Markdown allows for nested markup or any of the
  other nice features that were not present in Haddock at the
  time. This might have skewed their view that if Markdown is
  implemented, so will be those features

* Haddock had a lot of quirks and restrictions. While this ties in
  to the point above, this also meant that its rules were rather
  simple. Weird, but simple. This made for rather easy mapping to
  (a flavour of) Markdown. This is no longer the case however. For
  example, non-newline-separated list treatment is treated in a
  [large variety of ways](http://johnmacfarlane.net/babelmark2/?text%3D*%2BUnord%2B1%250A*%2BUnord%2B2%250A%250A999.%2BOrd%2B1%250A888.%2BOrd%2B2). What are we to do then? This raises the
  question of what to do for any features we might have, be it now
  or in the future that do not have Markdown equivalents? We
  either pile up more and more of our custom markup, effectively
  destroying the whole reason for this alternate syntax or
  restrict the features available which is certainly less than
  perfect. Adding any new features will be much more difficult
  not only because we have to now consider parsers for two markup
  languages but also because we have to any possible changes that
  we simply cannot implement in either one without drastically
  changing the syntax or semantics, something we try to avoid if
  it all possible.

Nevertheless, let us pretend that these problems don't exist and
let's try to consider some Markdown that we would be implementing
as part of this.

Reasoning
=========

Original suggestions and their solutions
----------------------------------------
First I consider the markup listed in the [original proposal](https://gist.github.com/Fuuzetsu/81253ba7d0c51ac88052):

* {-# HADDOCK Markdown #-} pragma

    It would allow us to separate any new additions from the old
  ones, allowing us to better represent Markdown syntax, without
  interfering with any regular markup. Whether this is actually
  necessary will hopefully become clear in the next few points.

* No need for single-quoted strings for code

    The proposal argues that we do not needed such strings as we can
  simply use backticks (which are prevalent throughout Markdown
  implementations as code delimiters). The idea is that the
  anything between backticks would be treated as code and linked
  automatically. That is:

    ```haskell
      -- | `(++)` appends two `String`s. More information
      -- can be found in the module `Data.String`.
      -- Here a small example: `"a" ++ "b"` = `"ab"`
      (++) :: String -> String -> String (++) = undefined
    ```

    This links back to the idea of unifying ‘'’ and ‘@’ and same
  arguments against it apply. Worse, this also unifies the idea of
  module names, usually delimited by ‘"’. This makes it
  incredibly ambiguous: ‘Data.String’ is a valid type and a module
  name and we have to way to discern which one the user meant. A
  little known fact is that one can already use backticks in this
  way to link identifiers. We could get rid of single-quotes at
  best, but we still can't put whole auto-linking code blocks down
  this way. This proposition is therefore to be considered already
  existing or not feasible to implement on its own, depending on
  how one looks at it. In either case, anyone familiar with
  Markdown can already use this without need for separate syntax
  mode.

* [Headers](http://daringfireball.net/projects/markdown/syntax#header)

    The proposal talks about implemented headers. Normally, a header
  in Markdown is implemented like so:

    ```
      # Header 1
      ## Header 2
      ###### Header 6
    ```

    The proposal points out a valid problem: this can very easily
  clash with the C preprocessor. An alternate syntax is proposed:

    ```
      Header 1
      =========

      Header 2
      ---------
    ```

    This syntax however only exists for the first 2 headings which
  is rather unsatisfactory, especially for modules dedicated to
  re-exporting functions and sectioning the documentation. Worse
  yet, from the implementation side, the headings in export list
  are actually just regular unordered lists, beginning with the
  ‘*’, /without/ the Haddock marker. We still can't use the first
  syntax as

    ```haskell
      module Main
      (
      {-
      # Top level
      -}
      main
      ) where

      main :: IO ()
      main = return ()
    ```

    is perfectly legal regular Haskell and could easily be a CPP
  directive and the second syntax is close to useless there:
  it'd be very difficult to get regular multi-line comment to
  properly get picked as a Haddock comment and we only get 2
  headings which, by far, not enough..

    An alternative way would be to replace the ‘#’ symbol with an
  alternative such as ‘=’. This however defeats the ‘if you know
  Markdown, you can just use that’ point.

    In conclusion, Markdown headings would be limited to 2 levels,
  be unusable in the export list and make the behaviour
  inconsistent, defeating the whole point.

With this the proposal ends. I'll throw in some more thoughts on
adding this separate syntax however, basing in on the [original
Markdown documentation](http://daringfireball.net/projects/markdown/syntax) for the features we would require, roughly
in the order of appearance.

Problems with everything else
-----------------------------

* Inline HTML

    Currently not supported by Haddock itself. This is not viable at
  all because XHTML backend is not the only one we need to
  translate to and so we can't insert verbatim tags. Maybe if we
  had some kind of markup… Like the existing Haddock markup!

* Automatic escaping of special characters

    Not Markdown specific and can be implemented in Haddock itself,
  for things such as URLs.

* Headers

    Already discussed.

* Blockquotes

    Haddock already has birdtracks. These will actually be turned
  into a block so that requirement is met too. In the context they
  are used in Haddock, it makes no sense to nest these.

* Lists

    Already supported. No nesting is currently allowed. While it
  might be implemented as per [ticket #27](http://trac.haskell.org/haddock/ticket/27), it's not Markdown
  specific and the syntax wouldn't be any different. The only
  difference here is that ‘+’ would be allowed as an extra symbol
  for unordered lists. Hardly warrants a separate syntax.

* Code blocks

    Markdown allows us to denote code blocks by indenting text 4
  spaces. Only very few implementations allow this text to not be
  on a separate paragraph as can be seen [here](http://johnmacfarlane.net/babelmark2/?text%3Dfoo%250A%2B%2B%2B%2Bbar%250Abaz#results). This is different
  from vanilla Haddock in that we indent the text instead of
  delimiting it with ‘@’. We will consider this as a valid point
  when we come to the conclusion.

* Horizontal rules.

    No such thing in Haddock nor is it planned. Further commented on
  in the conclusion.

* Links

    Markdown allows links as follows

    ```
    This is [an example](http://example.com/ "Title") inline link.

    [This link](http://example.net/) has no title attribute.
    ```

    Currently Haddock only supports links of the form ‘<link label>’
  and no title attribute. While this could be changed to allow for
  title attribute, as it is, we can't model the first style and
  would only be able to allow for the second style. Unfortunately,
  the second style clashes with definition lists. Note that there
  is no equivalent to definition lists in Markdown. There is no
  reasonable way to implement this without either getting rid of
  definition lists OR changing the syntax for either Markdown
  links (unacceptable) or the definition lists (highly undesirable
  and we'd also have to make up new syntax, unknown to neither
  current Haddock users or anyone familiar with Markdown. This
  defeats the point of easy writing to Markdown users). Reference
  style lists suffer from the same problem not only in one place,
  when using the link but also when defining it later. It is
  therefore not viable to introduce this syntax without changing
  the existing Haddock syntax, alienating anyone who knows it
  already and wants any of the niceties that Markdown could offer
  them (or not!).

* Emphasis

    Markdown treats asterisks (`*`) and underscores (`_`) as indicators
  of emphasis. Text wrapped with one `*` or `_` will be wrapped with an
  HTML `<em>` tag; double `*`'s or `_`'s will be wrapped with an HTML
  `<strong>` tag. We currently only use double ‘_’s to indicate
  strong text. We again have ambiguity problems with lists when it
  comes to `*`. `*foo*` is a valid list. `**foo**` is a valid
  header in export list. Same argument and pseudo-workarounds as
  in the ‘Links’ point apply: we'd have to change existing list
  syntax.

    In fact, it's [incredibly easy to see](http://johnmacfarlane.net/babelmark2/?text%3D*list%250A**emphasis**) that this causes problems
  just with the Markdown syntax, without even beginning to worry
  about the Haddock side of things!

* Code

    I have covered backticks already.

* Images

    As with links, we have a problem of there not being 1:1
  functionality mapping in vanilla Haddock. This could be easily
  fixed by adding the needed functionality. While we avoid the
  problems with lists that we had in links, this syntax would
  further reinforce the need to use the similar, ambiguous syntax
  for links, leaving us no alternative but to change the list
  syntax. It still suffers from the same problems with the
  reference style of linking.

* Backslash escapes

    Haddock already does this, although right at this moment (August
  25th 2013) some markup (images, URLs, definition lists)
  doesn't obey ‘\’ as it replicates the behaviour of the old parser. With
  the recent parser changes however, it's now trivial to provide
  this behaviour and it will most likely be done so: it's a sane
  default and considering the changes being made already, I think
  that this is the best time to introduce it, giving a heads-up to
  Haskellers to amend any of their documentation. I should note
  that any documentation using a ‘\’ in a markup that swallows it
  up was most likely intended to act as an escape and wasn't
  visually inspected.

* Automatic links

    Haddock already does this: it's just the vanilla link syntax,
  nothing to change here.

Conclusion
==========
I have went over every relevant feature from the
[original Markdown](http://daringfireball.net/projects/markdown/syntax)
and discussed suitability of a Markdown syntax mode in Haddock in
terms of these features. As it turns out, there is only one thing that
can reasonably be implemented: indented codeblocks. Yes, there are
other things such as automatic escaping of special characters however
they have nothing to do with Markdown itself, they are simply things
that would be nice to have in Haddock itself and require no actual
special syntax change. Would I therefore feel justified to introduce a
whole new pragma for Haddock, implementing codeblocks with indentation
instead of ‘@’s and butchering the rest of the original syntax?

Why is this the case? It seemed like such a good idea to a large
amount of people when proposals were initially being presented.
Even if you didn't like Markdown, there were plenty of other calls
for reST and Wiki syntax. It was going to be great: people don't
have to learn Haddock syntax and can concentrate on writing code
more. Why can't we have things like horizontal rules or inline
HTML? I think the first sentence in the Markdown documentation
after the introduction explains it pretty well: “Markdown’s syntax
is intended for one purpose: to be used as a format for writing for
the web.”. As it turns out, Haddock is not ‘the web’. It just
happens that most people see it in action once it's nicely rendered
into XHTML and up on Hackage. It in fact also has back-ends for
LaTeX and Hoogle! Does it make sense to have inline HTML tags in
LaTeX? No. Does it make sense to have horizontal bars in Hoogle?
No. Sure, you could argue that these backends could just ignore it
but it makes no sense to allow Markdown, used as a mid-point
between plain text and writing HTML by hand for Haddock. Haddock
already has its own markup structures that other back-ends
interface with, one of which happens to be for the web.

With all this in mind, I do not think that Markdown is something
that can be reasonably added to Haddock. We could very poorly try
to emulate it, butchering any existing syntax but the result would
be that people who know Haddock would have to look it up and would
make mistakes trying to write it due to changes AND people who know
Markdown would have to look it up and would make mistakes because
it's not possible to provide an implementation that even remotely
looks like what it does when writing something we'll directly
convert to HTML later.


Summary
=======
To summarise, Markdown can't happen simply because it's a format for
editing documents for the Web. It's just a mid-way point between
writing plain, unformulated text and writing HTML by hand. Many others
exist and were suggested, such as reST and Wiki Creole. They suffer
from the same problems, although often not as heavily.

While most people probably see Haddock generated documentation on
Hackage, it is __not__ what one might call a mid-point between HTML
and straight up text. In fact, LaTeX and Hoogle back-ends exist which
reconfirm this. Internally, the documentation comments are parsed into
a markup structure that's agnostic of which back-end it's going to be
used for. This makes adding back-ends possible. What does this mean
for us? This means that there is no clear mapping of Haddock features
to Markdown syntax.

Here we need to remember the reason for why Markdown was requested: it
would allow people who know Markdown to write documentation instead of
having to learn yet another syntax. This is also why it was more of a
markup popularity contest rather than the case of picking the most
suited tool for the job.

An easy example for why this goal can't easily be achieved is
definition lists: Markdown has no such thing. Worse yet, the
definition list syntax is used in Markdown for something totally
different, something we already have syntax for. More examples are
possible and general problems with Markdown also surface: see the
links to babelmark2 results in the reasoning sections.

This means that:
* We end up having to redefine existing Haddock syntax when using
  Markdown

  This makes it useless to anyone that already knows the Haddock syntax
but prefers Markdown

* We have to solve problems posed by Markdown itself

  This makes it horrible for any Markdown users as there exist tens of
flavours that solve these differently and when people speak of
Markdown, they actually speak of ‘the Markdown flavour that I use the
most’. This also means any users have to look up how we solved those
problems and what quirks this introduces, defeating the point.

* We have to introduce new syntax whenever features are added

  Same as above, defeats the point if users have to look things up. This
is also horrible for implementation side of things as we have to
effectively implement, test and maintain two separate parsers and
might miss out on features simply due to burden of implementation.

* We have to eliminate existing Markdown features.

  An easy example of this is inline HTML tags which Markdown allows.
This makes no sense for LaTeX and Hoogle back-ends.


  Again, while some of these things could be solved by picking a less
popular markup alternative (reST or Creole), in the end we have to
deal with the fact that these are not a 1:1 fit for Haddock,
effectively defeating the point of being able to just jump in. If they
are a 1:1 fit but everyone would have to learn it, they might as well
learn Haddock syntax: there aren't many entities and many of the
quirks people complained about have been or are being ironed out.

For these reasons, Markdown will not be implemented.


I will send out the link to this post on the Haskell café mailing list
so if you have any comments, please direct them there where everyone
can see: this is a project for the community after all.
