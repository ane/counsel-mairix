# ivy-mairix, an Ivy interface for the Mairix mail indexing program

ivy-mairix is an [Ivy](https://github.com/abo-abo/swiper) interface for rapidly going through [Mairix](https://github.com/vandry/mairix) search
results. By default, Mairix works by saving the search results into a separate
file, like a unix [Mbox](https://en.wikipedia.org/wiki/Mbox) file, and then you point your email program to that file
to display the result. This can be quite inconvenient, since you have to reopen
the search mailbox every time your search query changes. Using Ivy, we can now
interactively refine our search and view the results in real time.

Currently this is very much a **work in progress** but if you have the [Emacs
Interface for Mairix](https://www.gnu.org/software/emacs/manual/html_node/mairix-el/index.html) properly configured it should work out of the box by
invoking <kbd>M-x ivy-mairix</kbd>.

**Patches are welcome!** Please note that this package is intended to be a part
of GNU ELPA, thus for patches over >15 lines your FSF paperwork ought to be in
order.

## Demonstration

![image](./demo.gif)

## TODO stuff (if you feel like contributing)

  * support for threads (i.e. the prefix arg on `ivy-mairix`)
  * support for Gnus and VM frontends
  * `counsel-mairix` i.e. a version of `ivy-mairix` where the current buffer
    changes to display the selected mail
  * these could go into the ticket tracker

## Copyright & License

Copyright &copy; Antoine Kalmbach. Licensed under the GNU GPL version 3.
