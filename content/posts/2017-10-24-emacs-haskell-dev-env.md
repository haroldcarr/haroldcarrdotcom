---
title: emacs/haskell development environment
tags: emacs, haskell
---

This is my emacs/haskell development environment.

I use both [Intero](https://commercialhaskell.github.io/intero/) and [Dante](https://github.com/jyp/dante)

-   Intero only works with [Stack](https://docs.haskellstack.org/en/stable/README/)
    -   See: <https://github.com/cydparser/demo-emacs-haskell/>
-   Dante works with cabal, [nix](https://nixos.org/nix/), stack, etc.

Intero seems to work better with Stack than Dante.

Last Modified : 2017 Oct 26 (Thu) 08:21:13 by Harold Carr.

<!-- MORE -->

---


# setup

Get

-   `.haskell-minimal-init.el`
-   `hc-haskell-dante.el`
-   `hc-haskell-intero.el`
-   and the files referenced in `.haskell-minimal-init.el`

from <https://github.com/haroldcarr/emacs>.

When the following starts, it will ask you whether to use Dante or Intero.

    emacs -q -l <path to>/.haskell-minimal-init.el

---


# [projectile](https://github.com/bbatsov/projectile)

project navigation, building, testing, &#x2026;

Keybindings (subset)

-   `C-c p !` : projectile-run-shell-command-in-root
-   `C-c p c` : projectile-compile-project
-   `C-c p f` : projectile-find-file
-   `C-c p k` : projectile-kill-buffers
-   `C-c p t` : projectile-toggle-between-implementation-and-test

To see full list, if [whick-key](https://github.com/justbur/emacs-which-key) is install: press `C-c p` and wait.

try:

-   go to any directory in a project
-   `C-c p f`
-   type a filename from anywhere in the project
-   use `C-p` and `C-n` to navigate list of candidates
-   press `RET` to go to that file

---


# [yasnippet](http://github.com/joaotavora/yasnippet) / [haskell-snippets](https://github.com/haskell/haskell-snippets)

some templates (to see full list: `M-x yas/describe-tables`)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">`mod`</td>
<td class="org-left">adds a named (based on filepath) module declaration</td>
</tr>


<tr>
<td class="org-left">`main`</td>
<td class="org-left">adds a `Main` module and `main` function</td>
</tr>


<tr>
<td class="org-left">`lang`</td>
<td class="org-left">adds a `LANGUAGE` pragma</td>
</tr>


<tr>
<td class="org-left">`opt`</td>
<td class="org-left">adds a `OPTIONS_GHC` pragma</td>
</tr>
</tbody>
</table>

try:

-   open new file `List.hs`
-   type `mod`
-   press `M-/` (hippie-expand)
-   select `simple module`
-   press `RET`
-   press `tab` to accept default module name (or start typing)

---


# haskell

---


## haskell-hoogle (part of [haskell-mode](https://github.com/haskell/haskell-mode))


### search by name

try:

-   in a haskell file, point to something provided by external library or GHC
-   `M-x hayoo` (using [Hayoo!](http://hayoo.fh-wedel.de/))
-   `M-x hoogle` (using [Hoogle](https://www.haskell.org/hoogle/))


### search by type

try:

-   `M-x hayoo` or `hoogle`
-   enter : `f a -> Maybe c` RET

---


## Dante / Intero

---


### view types / info

try:

**types**

-   point to something
-   `C-c .`   : dante-type-at
-   `C-c C-t` : intero-type-at

**info** (shows definition and where defined, even if external)

-   point to something
-   `C-c ,`   : dante-info
-   `C-c C-i` : intero-info

---


### definitions and references


#### definitions

-   point to something
-   `M-.` : jump to definition (both dante and intero)
-   `M-,` : return to previous location (both dante and intero)


#### references

-   point to something
-   `M-?` : xref-find-references (dante : TODO : DOES NOT WORK FOR ME)
-   `M-?` : intero-uses-at (TODO : DOES NOT WORK FOR ME)

---


### apply suggestions / auto-fix

try:

-   add the following code to `List.hs`

```haskell
    data List a = Cons a (List a) | Nil
      deriving (Eq, Foldable, Show)
```

-   see red squiggly line under `Foldable`
-   `C-c ! l` : flycheck-list-errors
-   says "&#x2026; You need DeriveFoldable &#x2026;"
-   put cursor on `Foldable`
-   `C-c /`   : dante-auto-fix
-   `C-c C-r` : intero-apply-suggestions
-   (inserts appropriate `LANGUAGE` pragma)

---


### add type info

try:

-   add to `List.hs`

```haskell
    cdr Nil = Nil
    cdr (Cons _ xs) = xs
```

-   see red squiggly line under `cdr`
-   `C-c ! l` : flycheck-list-errors
-   says "&#x2026; Top-level binding with no type signature &#x2026;"
-   `C-c /` : dante-auto-fix
-   `C-c C-r` : intero-apply-suggestions
-   (inserts appropriate type signature)

---


### doctest

try:

-   add to `List.hs`

```haskell
    -- | Returns the first element, if non-empty.
    --
    -- >>> car Nil
    --
    -- >>> car (Cons 'a' Nil)
    car :: List a -> Maybe a
    car xs = case xs of
      Nil      -> Nothing
      Cons x _ -> Just x
```

-   point inside comment
-   `C-c "` : dante-eval-block
-   (inserts function call results)

---


### process buffer

If dante starts acting weird, restart it.

    M-x dante-list-buffers RET
    M-x intero-list-buffers RET
    d ;; mark process for deletion
    x ;; kill it
    q ;; quit process list
    M-x dante-restart RET
    M-x intero-list-buffers RET

---


# completion via [company](https://company-mode.github.io/)

try:

-   in `List.hs`
-   type : `import System.E`
-   `M-n` or `M-p` : move through suggestions
-   note: delete `E` until only : `import System.`
-   now shows all packages in `System`

---


# projectile / test files

try:

-   from `List.hs`
-   `C-c p t`
-   finds and jumps to (or creates) `ListSpec.hs`

---


# projectile / build and run

-   `C-c p c` : build
-   `C-c p !` : run

---


# formatting

-   `M-x haskell-mode-stylish-buffer`

