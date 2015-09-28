# Scarlet

Scarlet is a lightweight blogging engine built on [Yesod][1], based on the design of [Stirling][0].
More features (by which I mean any) will be forthcoming, and when they are, I’ll update this readme.

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
