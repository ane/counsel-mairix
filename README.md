# counsel-mairix, an Ivy interface for the Mairix mail indexing program
[![builds.sr.ht status](https://builds.sr.ht/~ane/counsel-mairix.svg)](https://builds.sr.ht/~ane/counsel-mairix?)

counsel-mairix is an [Ivy](https://github.com/abo-abo/swiper) interface for rapidly going through [Mairix](https://github.com/vandry/mairix) search
results. It lets you search your emails interactively in the minibuffer, results
update as you type. Additionally, if you're looking at an email, you can quickly
insert search terms based on the email. Key features of counsel-mairix are:

  * **Fast interactive searching**. Mairix is really fast, and the results update
    nearly instantly.
  * **Refining your search**. You can quickly insert relevant bits of messages to
    become part of the search string.
  * **Simplicity**. Mairix is really [really easy to configure](https://www.gnu.org/software/emacs/manual/html_node/mairix-el/Configuring-mairix.html), and Emacs has
    [built-in support for it](https://www.gnu.org/software/emacs/manual/html_node/mairix-el/index.html). 

By default, Mairix works by saving the search results into a separate
file, like a unix [Mbox](https://en.wikipedia.org/wiki/Mbox) file, and then you point your email program to that file
to display the result. This can be quite inconvenient, since you have to reopen
the search mailbox every time your search query changes. Using Ivy, we can now
interactively refine our search and view the results in real time.

Currently this is very much a **work in progress**. counsel-mairix depends on the
[Emacs Interface for Mairix](https://www.gnu.org/software/emacs/manual/html_node/mairix-el/index.html) which is bundled with Emacs. It should work out of
the box by invoking <kbd>M-x counsel-mairix</kbd> if you have configured it properly (see
[Installation](#installation) below).

**Patches are welcome!** Please note that this package is intended to be a part
of GNU ELPA, thus for patches over >15 lines your FSF paperwork ought to be in
order. Please mail patches to my [public inbox](https://lists.sr.ht/~ane/public-inbox) at [~ane/public-inbox@lists.sr.ht](mailto:~ane/public-inbox@lists.sr.ht).

## Demonstration

*Mairix running on (mostly) the [September 2020 archive of emacs-devel@gnu.org in Mbox format](https://lists.gnu.org/archive/mbox/emacs-devel/2020-09)*.

![image](./demo.gif)

## Installation

A prerequisite on the system is obviously that Mairix is installed and
configured properly. See [(mairix-el) Configuring mairix](https://www.gnu.org/software/emacs/manual/html_node/mairix-el/Configuring-mairix.html). 
 
Since this is still alpha quality software, it is not available in any package
repository. Thus clone it somewhere in your system and add it to your load path:

``` emacs-lisp
(add-to-list 'load-path "<path-to-this-repo>")
```

You can do this using [use-package](http://github.com/jwiegley/use-package):

``` emacs-lisp
(use-package counsel-mairix
  :load-path <path>)
```

If you have the [Emacs Interface for Mairix](https://www.gnu.org/software/emacs/manual/html_node/mairix-el/index.html) configured properly then this should
work without any additional configuration. Otherwise, you need to configure it.
For example, if you use Mairix with Mbox format results, configure it like this:

 ``` emacs-lisp
(setq mairix-file-path "~/mail")        ;; The folder where the search file is stored
(setq mairix-search-file "search.mbox") ;; Name of the search file itself
```

## Functionality

The table below lists a summary of the functions in counsel-mairix.  Please
refer to their documentation in Emacs for more information.

| Function                       | Description                                                     |
|:-------------------------------|:----------------------------------------------------------------|
| `counsel-mairix`               | Run mairix queries interactively                                |
| `counsel-mairix-save-search`   | Save your searches from your previous `counsel-mairix` searches |
| `counsel-mairix-search-from`   | Start a `counsel-mairix` search using the `From:` header        |
| `counsel-mairix-search-thread` | Start a `counsel-mairix` search using the `Message-Id:` header  |

## Keybindings in the Ivy search

| Binding              | Description                |
|:---------------------|:---------------------------|
| <kbd>C-c C-t</kbd>   | Toggle threading           |
| <kbd>C-c C-s i</kbd> | Insert saved mairix search |
| <kbd>C-c C-s s</kbd> | Save current search        |

If you're viewing an email in one of the major modes Mairix supports, you can
hit these bindings to add them to your search string:

| Binding              | Description                                           |
|:---------------------|:------------------------------------------------------|
| <kbd>C-c C-f f</kbd> | Yank the `From` value as `f:...` in your search       |
| <kbd>C-c C-f s</kbd> | Yank the `Subject` value as `s:...` in your search    |
| <kbd>C-c C-f t</kbd> | Yank the `To` value as `t:...` in your search         |
| <kbd>C-c C-f i</kbd> | Yank the `Message-Id` value as `f:...` in your search |

Remember that the default binding <kbd>M-j</kbd> (`ivy-yank-word`) to yank
things your cursor is on to insert it into the minibuffer.

## Customization

counsel-mairix defines the following customization variables:

### Custom variable: `counsel-mairix-mail-frontend`

Override the mail program used to create the Ivy search buffer. 

This setting can override which mail program you are normally using for Mairix
searches defined in [`mairix-mail-program`](https://www.gnu.org/software/emacs/manual/html_node/mairix-el/Extending.html). This can be useful if you want
traditional Mairix searches to use some mail program (e.g. Gnus) but prefer
another program to display the Ivy buffer.

It defaults to the value of `mairix-mail-program`.

``` emacs-lisp
(setq counsel-mairix-mail-frontend 'rmail)
```

This would force counsel-mairix to use Rmail for displaying the search buffer.

### Custom variable: `counsel-mairix-include-threads`

Whether to prompt for including threads in the Mairix search. Mairix.el defaults
to querying every time, but I found this behaviour annoying.

Set it to

  * `'prompt` to ask every time (the default)
  * `t` to always include threads
  * `nil` to never include threads.

## TODO stuff (if you feel like contributing)

**Gnus** and **VM** aren't supported as display frontends.

These could go into the ticket tracker.

## Copyright & License

Copyright &copy; Antoine Kalmbach. Licensed under the GNU GPL version 3.
