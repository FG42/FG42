<div align="center"><img src="https://assets.gitlab-static.net/uploads/-/system/project/avatar/8205594/fg42.png?width=70" alt="FG42" align="center" /></div>
<a href="https://gitter.im/FG42/FG42?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge"><img src="https://badges.gitter.im/FG42/FG42.svg" /></a>
<br/><br/>

#  Future Gadgets 42
If you love **GNU/Emacs**, If you love to program in an editor instead of big slow IDE but still enjoy to
have a handy code editor then **FG42** is the right choice for you.

If you need help, or even want to just say hello, try our **IRC** channel on freenode: **#5hit**.

## Download
* [Latest Stable - 2.31.2](https://gitlab.com/FG42/FG42/-/jobs/artifacts/2.31.1/raw/fg42.2.31.2.tar.gz?job=build-package)
* [Latest Master](https://gitlab.com/FG42/FG42/-/jobs/artifacts/2.31.1/raw/fg42.master.tar.gz?job=build-branches)

## Dependencies

In order to run **FG42** you need **GNU Emacs >= 25**. FG42 uses several extensions internally
which each of the has different external dependencies. To gain more information about external
dependencies ( If we failed to tell you inside the **FG42** ) just run the `describe-extension`
command.

## Install pre built package

Download one of the prebuilt packages and follow these instructions:

```bash
# extract the content of the tarball in your home directory
$ tar zxvf <downloaded package>.tar.gz -C ~/
# You can clone it where ever your want
$ cd ~/.fg42/
$ make install
# Run fg42, It will download and build some lisp dependencies on the first execution.
$ fg42
```


## Install from source

In order to install **FG42**, issue the following commands:

```bash
# clonse the FG42 repository in ~/.fg42 directory
$ git clone https://gitlab.com/FG42/FG42.git ~/.fg42/
# You can clone it where ever your want
$ cd ~/.fg42/
$ make install
# Run fg42, It will download and build some lisp dependencies on the first execution.
$ fg42
```
Since the installer script uses `sudo`, during the installation process,
you'll have to enter your password. Make sure that you're user account has
a `sudo` access.

If you suffer from poor internet connection (like people in my homeland),
you can also download the tarball from release section and extract the tarball
in your `$HOME` directory. So you're going to end up with `.fg42` directory in
your `$HOME` and all you have to do is to install it using `make install` command.

**NOTE**: Please note that the tarball releases are still experimental and might have
some issues since they contain the elisp packages as well. In case of any problem
please report the issue.

Enjoy using **FG42** ;)

## Configuration

After installation there would be a file at `~/.fg42.el` which is the user specific
configuration of **FG42**. You can configure your copy of **FG42** using this file.
Also you can generally use this file to configure Emacs as well.

## Terms & Concepts
FG42 introduced some new features to the Vanilla Gnu/Emacs. So you migh encounter several of these features during your
journey. It's a good idea to at least know what you're dealing with.

### Extensions
FG42 created around the idea of extensions. Each extension extends the FG42 functionality for a certain language or use case.
For example `clojure` extension adds support for a integrated development environment for `clojure` language. All the configuration
related to this environment leave inside that extension. By default, all the extensions which ships with FG42 are enabled. In order
to disable any extension which you might not need, just comment them out in your user configuration file in `~/.fg42.el`. For example
here is my own list:

```lisp
(activate-extensions 'editor
                     'development
                     'web
                     'editor-theme
                     'javascript
                     ;;'ruby
                     'clojure
                     ;;'php
                     'python)
                     ;;'arduino

```

In order to learn more about each extension you can use `M-x describe-extension`.

### Abilities
Abilities are the smallest piece of configuration which add support for a certain functionality to **FG42**. Each extension contains several abilities.
Using abilities you can choose the set of abilities you like and disable those which you don't like. For example `helm` and `ido` are two different
library which conflicts with each other by we have an ability for each of them in the system, so you can choose which one to use and disable the other one.

In order to disable any ability, just add them to your configuration file in `~/.fg42.el`. For example my current disabled abilities are:

```lisp
(disable 'rbenv-global 'helm 'spell 'linum  'ivy 'smart-mode-line 'desktop-mode)

```

In the above list I disabled `helm` because I want to use `ido`. At the moment there is no way to find out about all the enabled abilities at runtime. Or
discover what abilities present in any extension (You can take a look at extension docs using `describe-extension` command) at runtime. But I'll add something
later :P.

## Documentation

If you're familiar with **GNU/Emacs** you're not going to have a problem with getting
help. But just to clrearify here is a list of keybindings that may guide you during
your experience:


| Key bindings | command | Description      |
|:------------:|--------:|------------------|
| `C-?`        | cheatsheet-show | Quick cheatsheet |
|              | describe-extension | It's going to ask you an extension name and show you the documentation of that extension |
| `C-h m`      | describe-mode | All the information and keybindings of your current modes. e.g open of a `ruby` file and use the this keybinding to take a look at all the keybinding of ruby extension |



## How to implement your own extension:

In this part we will explore the extensibility features of the FG42. As an example
we will add LaTeX autocompletion and other useful features to our emacs based on AUCTeX
and company-auctex [company-auctex](https://github.com/alexeyr/company-auctex).
We can install the required package manually by ``` install M-x package-install RET company-auctex RET ``` or the FG42
would do this for us based on its configurations.
Assume that our extension is called latex, to this end we are needed to create a file and a directory in the following
directories:

```bash
   yourfg42home/lib/extensions/latex.el
   yourfg42home/lib/extensions/latex/init.el
```

First we are going to explore the latex.el. your file needs to include these parts:
```lisp
(require 'fpkg)
(require 'fg42/extension)
(require 'extensions/latex/init)
```

extensions/latex/init says to FG42 that the corresponding init file is in the latex directory and in the init.el file.
after that, you must add the MELPA dependencies you wish to install and use, in our case:
```lisp
(depends-on 'company-auctex)
```
There are other doc and extension functions you may like to implement for your versioning and documentation stuff.
finally you have to provide your extension using:
```lisp
(provide 'extensions/latex)
```
At this point our latex.el is done and we have to implement the init.el file. extension customization, key-binding, and hooks on the
major modes, etc. are configured in init.el. The only protocol you have to recall is that name of your init function should
follow this pattern: lisp extensions/latex-initialize() and you have to provide it at the end.

for example:
```lisp
(defun extensions/latex-initialize ()
  ;LaTeX development initialization
(require 'company-auctex)
(add-hook 'latex-mode #'company-auctex-init))

(provide 'extensions/latex/init)
```

FG42 will add your package to its classpath and would load it on the related major mode.
Finally you have to enable this extension in the yourfg42home/fg42-confilg.el file.
```lisp
(activate-extensions 'latex etc)
```

you are done. enjoy using your extension!

## Latex-lsp mode

FG42 has a builtin support for LaTeX language. In the latest version Microsoft lsp-latex language server has been used for this purpose. You need to download the [texlab](https://github.com/latex-lsp/texlab/releases) binary for your operating system and put it in your class path (e.g /usr/bin/texlab). If FG42 didn't load the server automatically, you can run the server with the following command:
```
M-x latex-run-lsp
```

## Debugging
If you ran into an issue and want to debug FG42, the best and easiest way is to turn on
debugging by uncommenting `(setq debug-on-error t)` in your configuration (`~/.fg42.el`)
and restarting FG42. After that you'll get a traceback for any exception in FG42.

## what's with the name?
I'm a huge fan of [Steins Gate](https://en.wikipedia.org/wiki/Steins;Gate) anime and I follow its
naming convensions on **Future Gadgets 42**.

## Why we moved from Github ?
We're not happy with Microsoft buying Github and we don't trust a company like Microsoft because of their history and
their strategies (For more information take a look at ([Halloween documents](https://en.wikipedia.org/wiki/Halloween_documents)).
So we decided to move to Gitlab as an alternative and we are happy here so far.

# License

  FG42 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
any later version.

  FG42 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

  All the documents of FG42 that locate in 'doc' directories release
under the term of GNU FDL.

Copyright (C) 2010-2018  Sameer Rahmani <lxsameer@gnu.org>
