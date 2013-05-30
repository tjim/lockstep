# LOCKSTEP: pair programming in Emacs

Lockstep is a package for pair programming in Emacs.  It synchronizes
the windows and points of two or more Emacs frames, so that a team of
programmers can share an editing session.  All programmers see the
same buffers and live edits, and any programmer can take over the
editing session.

Lockstep requires Emacs 24.

## Installation

Place lockstep.el in your load-path and

   (require 'lockstep)

## Use

One programmer starts an Emacs server and the other programmer(s) join
the session with `emacsclient`.  Programmers must execute

    M-x lockstep

to start synchronizing their frame.

*BE CAREFUL:* **any** programmer in the session will be able to
control the Emacs process running as the user who started the Emacs
server.  That means that any programmer in the session can edit files
and start processes as if they were that user.  The best way to deal
with this is to create a special account solely for the purpose of
pair programming, give this account limited permissions, and let all
of the programmers log in to the account.

Then the first programmer can start the session with

    emacsclient -a '' -c

for a graphical Emacs, or

    emacsclient -a '' -t

for an Emacs terminal session.  The first programmer must also do

    M-x lockstep

Then other programmers can join by running

    emacsclient -a '' -c

for a graphical Emacs, or

    emacsclient -a '' -t

for Emacs in a terminal, followed by

    M-x lockstep

Any programmer can leave the session simply by killing their frame;
the session keeps running for the other programmers.

Remote programmers will probably need to use a Emacs in a terminal,
unless they have an unusually fast network connection.

## Features

* No additional program (screen/tmux/dtach) required, just Emacs.

* Both graphical and terminal Emacs are supported, simultaneously.

* Two or more frames can be synchronized.

* Synchronized frames can be mixed with unsynchronized frames in the same Emacs instance.

* Frames can be resized independently; frames do not have to have the same size.

## Caveats

* Requires Emacs 24

* The windows and points of the frames will be synchronized, but not
  the window configurations.  This means that windows may be arranged
  differently in two synchronized frames.

* Frame minibuffers are not synchronized.

## Options

* By default lockstep does not synchronize popups.  If you want to synchronize popups,
e.g., for auto-complete mode, then do

    (lockstep-popup)

## Authors

Trevor Jim
