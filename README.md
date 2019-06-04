Skef
====

*NB: This is still work in progress and not useable at the moment.*

Skef is a small language and tool for keeping and organizing lab notes or a
personal journal. 

Skef allows you to have all your notes in one plain text file.  Using labels
you can order your entries according to date and categorize them.  You can
create todo-lists, track your time..etc. Using the *Skef* command line tool you
will be able to filter and search through your notes, generating CSV or JSON
files for external processing. You will be able to use the command line tool to
quickly generate a list of "open" todo's, or generate time tracking sheets for
certain months or projects.

For some background on the how and why, you can read my
[blog post](https://jelle.xyz/posts/2016-11-06-skef.html).

Here's an example Skef file:

```
[Fri Sep 02 2016 - 2016-09-02]
    This is a line of text that is just a general little untaxonomized line of
    text that will be filed under September 2nd.

    [project]
        This text is filed under "project". I will be able to run queries that
        gather all my entries filed under this tag.

        [subproject]
            You can even nest projects, nifty wifty.

        [todo mow the grass]

        As you can see above here, we're able to setup todos. The basic idea is
        that you'll be able to run the Skef commandline utility that retrieves
        all todos for a project, or simply all of them in one go. It outputs
        these todos on the commandline and you'll be able to setup your favorite
        text editor in such a way that it's piped into a new editor
        buffer.

        [todo bft]
            Sometimes you'll need more space to explain what to do. You can
            create a whole lot of text under one todo if you like, but you
            probably won't, because this means a lot of things that will require
            taking your lazy bum of the couch.

        [time 4.5h pondering about mowing the grass]

        We can have entries for tracking time. Skef will be able to retrieve all
        these time entries, output them as a CSV, or on the commandline for easy
        viewing.

        [done noticed the relatively high grass length]

        After you're done with a "todo", you can simply replace "todo" with done
        and it'll be marked as done :-). You can also use "skip" or "postpone",
        but the Getting Thing's Done philosphy says you shouldn't (which we
        don't care about of course, silly busy people).
```

Requirements
------------
Right now, this project builds on Linux. Most of the BSDs should probably work
too. 

You will need to install the Haskell Platform on your machine. Find out more at
[https://www.haskell.org/platform/](https://www.haskell.org/platform/).

Building
--------
To build this you can use Cabal. In your terminal go to the root folder of this
project and run:

```
cabal sandbox init
cabal update
cabal install --only-dependencies
cabal build
```
