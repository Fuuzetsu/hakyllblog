---
date:   2014-03-18 17:38:28
title:  GSoC 2014 submission
author: Fūzetsu
---

Again we have come to the time where Google Summer of Code rolls
around. While I have already written up, posted up for public viewing
and submitted my proposal for this year, I have totally forgotten to
post it here for those of you who might not follow the mailing lists!

Firstly I'd like to remind you that the proposal deadline is March
21st, 19:00 UTC so if you still haven't come up with anything and
would like to participate, you should start to hurry. Come to
 #haskell-gsoc on Freenode if you're looking for help or feedback.

Secondly, I'd like to hint at possibly a second proposal which would
enable you to view documentation inside GHCi and could speed up
Haddock dramatically. This is still in ‘just an idea’ stage and
involves storing the documentation strings in the .hi files. Check
ghc-devs mailing list if you're interested in it, have comments, or
perhaps want to be the one to do this as a project!

Having said all that, below is my proposal in full as I submitted it not
that long ago. You can check haskell-cafe for a thread with discussion
about this proposal.

Yi concurrency, usability and hackability
------------------------------------------

* What is the goal of the project you propose to do?

    There are two main goals of the project: the first is to implement
    concurrency in the Yi text editor. The second aim is to start
    bringing Yi into the territory of usable and hackable editors.

* In what ways will this project benefit the wider Haskell community?

    While the project itself isn't one of the core ones (such as GHC,
    Haddock and Cabal), I feel that there are a couple of benefits to the
    community:

    1. Haskellers come closer to escaping the ELisp/vimscript hell. We
       can get a nicer programming environment, made and extensible in
       the language of our choice and get to use all the libraries
       that we're used to while we're at it.

    3. We'll have more Real World™ Haskell applications. On a more
       serious note, it can serve as a good example of how to do
       certain things with Haskell: off the top of my head, it
       demonstrates the use of dyre and gtk2hs in a real-world
       scenario rather than a 5 line example on the Haskell wiki. If
       the project is successful, we can add concurrency to this.

    3. Work on Yi (now and in the future) will undoubtedly spawn new
       Haskell libraries usable in other projects. My personal
       experience with Yi shows that it's actually very comfortable to
       write a generic library which does what we need and then having
       a separate package which uses the library to actually interact
       with Yi.


    Other than the Haskell community in general, this project should
    benefit anyone with some interest in text editors. I think it's
    safe to say that happens to be a large majority of Haskellers:
    most of us want nicer integration with Haskell tools and
    libraries[citation needed] and now it'll be possible through
    direct, type-checked library access.

* Can you give some more detailed design of what precisely you intend
  to achieve?

    The concurrency goal will involve careful study of Yi's inner
    workings in order to try and accommodate concurrency. It might
    come as a surprise to many but currently Yi has no such support.
    There are various ways to do concurrency and the first part of the
    project will concentrate on settling for one. An example of two
    different ways is to extend the existing Yi engine with classical
    tools (MVars, channels) to accommodate for concurrency that way.
    An alternative way would be to modify the engine so that
    concurrency support is natural. Such experiment was started
    [here](https://github.com/ethercrow/y) using the sodium FRP
    package which would give us concurrency ‘for free’. The experiment
    is not complete and this is the kind of thing that will first be
    explored.

    Of course once we settle for a method, time will be spent
    implementing it. In the end, this should allow us to do things
    such as fire Yi events periodically or do network transfers
    without having to halt the whole editor. Editors such as emacs
    which are single-threaded effectively hop back-and-forth between
    tasks on a single thread. We aim to provide the ability to simply
    have tasks on different threads which allows us to take advantage
    of system resources much better.

    The second part of the project is to make Yi more usable and
    hackable. Usability here involves fixing bugs apparent to the user
    and hackability involves bugs apparent to developers. Further,
    as part of usability, I plan to implement as many editor modes as
    I find time for.

    Specifically, here are some open bugs that I hope to either fix or
    to make a considerate progress on: #445, #397, #517, #519, #515,
    #516, #513 (concurrency), #512, #507, #504, #502, #501, #499,
    #497, #493, #487, #478, #477, #468, #465, #399, #396, #391, #390,
    #382, #322, #295, #172, #160, #106, #145, #112, #82, #509.

    All the bug numbers can be viewed on
    [GitHub](https://github.com/yi-editor/yi/issues/). Please note
    that some of these are documentation bugs: Yi suffers from poor
    documentation and I believe that's what the main problems in
    gaining developers and users has been. When time or area I'm
    working on allows, missing documentation will be written.

    If I find any issue that have been fixed or are no longer
    applicable, the reports will simply be closed. The issues are very
    varied: unicode problems, keymap problems, highlighter problems,
    reloading problems, testing problems, mode problems… There is
    certainly enough work to entertain anyone for a longer amount of
    time while making Yi visibly better.

    The list of issues is simply an indicator of which problems the
    second goal of the project will concentrate on, rather than as a
    promise of which bugs are guaranteed to be fixed by the end of it.

    Alongside this goal, I'll write any modes for Yi as I find time
    for them. The completion of concurrency part of the project allows
    us to write many of the modes frequently requested by people
    wishing to use Yi which are currently impossible/unfeasible to
    write.

* What deliverables do you think are reasonable targets? Can you
  outline an approximate schedule of milestones?

    The plan is based on the GSoC time line:
    20 April - 19 May – while this is a bonding period, I'm already a
    part of the Yi community and have a fair grasp of it. I'd start to
    look into this project as early as this period (and in fact I plan
    to make steps towards it before this date which means some of the
    outlined issues might get fixed early ;) ).

    19 May - 23 June – coding period; by this point I expect to have
    decided on which concurrency model we'll use and have a good idea
    of how it'll be implemented. By the end of this period,
    concurrency should either be completed or nearly done, depending
    on any unexpected problems that might come up. The deliverable
    would be Yi with (at least some) concurrency support.

    24 June - 11 August – second part of the coding period; work on
    any of the listed (or unlisted bugs) and finish up concurrency if
    it is still not done. Write extra Yi modes, libraries and
    documentation as time allows.

    11 August - 18 August – post-coding period; write any missing
    documentation, promote any cool new stuff we wrote ;) While I can
    not think of a specific deliverable, many bugs should now be
    fixed, Yi should have a lot more documentation, tests and modes.

    As a final note regarding the time line, it is not strictly
    necessary that the project implements concurrency first: while
    some bugs might need such support, many simply do not. If it's
    convenient to fix something that I had originally planned to for
    the second part of the project, I'll do so.

* What relevant experience do you have? e.g. Have you coded anything
  in Haskell? Have you contributed to any other open source software?
  Been studying advanced courses in a related topic?

    Second year CS student. I program on regular basis using Haskell.
    I contribute to a bunch of FOSS projects as it seems necessary
    (see [my GitHub](https://github.com/Fuuzetsu)).
    I have successfully completed GSOC in 2013 which involved working
    on Haddock. To this day I help out with Haddock which often
    involves looking at the large GHC code base.

* In what ways do you envisage interacting with the wider Haskell
  community during your project? e.g. How would you seek help on
  something your mentor wasn't able to deal with? How will you get
  others interested in what you are doing?

    I have a [blog](http://fuuzetsu.co.uk/blog) which gets propagated
    onto Haskell Planet. I'm active on IRC and many Haskell-related
    mailing lists. IRC, mailing lists and any relevant literature is
    where I'd seek help were I to get stuck on something my mentor
    can't help me with. I find that news about Yi are very popular and
    get propagated by the community itself very easily so I doubt
    there will be any problem getting people interested.

    I'm very easily reachable over e-mail and IRC and all the
    development is done in public.

* Why do you think you would be the best person to tackle this
  project?

    I've been interested in Yi for a couple of months and have already
    wrote some commits, closed quite a few issues and filed even more
    issues on my own. I have access to the Yi repository and
    I help anyone looking to get started with Yi. I have about 2 years of
    Haskell experience and had my fair share of staring at library
    code.

    As mentioned before, I'm active as a member of the community and
    help out with one of the core Haskell projects (Haddock).
