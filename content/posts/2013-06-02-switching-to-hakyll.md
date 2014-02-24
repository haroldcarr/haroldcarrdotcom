---
title: Switching to Hakyll
tags: hakyll, haskell, pandoc, emacs, org-mode, markdown
---

I chose [Hakyll](http://jaspervdj.be/hakyll/) for blog software
because it is written in [Haskell](http://www.haskell.org/) and it
is based on the file system instead of a database.  Also, it uses
[Pandoc](http://johnmacfarlane.net/pandoc/), John MacFarlane's
document conversion library (also written in Haskell).

<!-- MORE -->

I write posts in [Emacs](https://en.wikipedia.org/wiki/Emacs)
[org-mode](http://orgmode.org/), export to
[markdown](http://en.wikipedia.org/wiki/Markdown), then run the
result through Hakyll/Pandoc.

This site design is copied from [Travis
Brown's](http://meta.plasm.us/about/).  The configuration code also started with his version on
[github](https://github.com/travisbrown/metaplasm).

See [other sites](http://jaspervdj.be/hakyll/examples.html) using Hakyll.

My previous blog software was [Blosxom](http://en.wikipedia.org/wiki/Blosxom).
