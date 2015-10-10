# Scarlet

**Scarlet** is a lightweight blogging engine built on [Yesod][1], based on the design of
[Stirling][0]. It uses [jQuery][2] and [Bacon][3] on its Javascript side. Scarlet is released under
the MIT license (if it’s important to anyone), which matches jQuery, Bacon, and Yesod.

## features ##

* When you go to `/` on Scarlet, it serves up an infinite page of all the posts you have, loading
  new ones as you scroll down. It drops in anchor links every “page” so that you can return to that
  point.

* When you go to `/something` on Scarlet, it tries to find a post with a URI matching `something`.
  In the case of collisions, you are having a bad time and will not be going to space today, by
  which I mean “there are some bugs I need to work out on this.”

* Seriously, you probably shouldn’t use it at this point, but it’s kind of fun to play around with.
  I hope that anyone else has as much fun playing with this as I have!

## installing ##

To install and run Scarlet (which is still in a very unstable form!):

```
git clone https://github.com/rhwlo/Scarlet.git
cd Scarlet
cabal run scarlet
```

You may find it useful to create some test posts — I made a set of test posts and ran
`for post in test-posts/*; do cabal run scarlet-post $post; done`, which populated it pretty well.

## scarlet-post ##

Because no one ever wants to post via the Internet (why would they? It’s so insecure!), Scarlet
doesn’t let you do that. Scarlet knows better than anyone else ever could, naturally.

Instead, there’s an inconvenient update mechanism called `scarlet-post` which reads a file formatted
like so:

```
<!--title: a title-->
<!--lang: en-->
Here’s some content in Markdown. Whee!
```

and then transforms it, entirely believably, into a database entry. It’s probably actually easier to
just use `sqlite3` to directly modify the file.

[0]: https://github.com/celoyd/stirling
[1]: http://www.yesodweb.com/
[2]: http://jquery.com/
[3]: https://baconjs.github.io/
