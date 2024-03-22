# my ~/etc directory

## New tab-oriented emacs config
Check out [emacs.el](emacs.el), I revamped my emacs config.
I decided against using spacemacs (too bloated) and even
evil-mode. My new config is pretty basic, but it adds a
system for keeping track of notes:


### Notes system
There are two files: ~/Desktop/past.org and ~/Desktop/today.org
The today file is concatenated to the past file, and then today is
emptied and set up with today's date. This is done with the `(idempotent-today-move-to-past)` function.

This is a simpler system than my old devlog.org which attempted to keep everything in one file. It also avoids the complexity of org-roam. While I love org and I love org-roam, it was a mess to use, and I already just use Roam for my knowledge graph. I want dumb files.

This is nice because it maintains one file, but the working file is small and minimal.

### Tabs
The `tab-bar-mode` is the thing I missed most about doing a lot of work in emacs. I could manage projects with projectile, but I need a visual like the tab bar. This hits the spot.

I also made a `Super-t` shortcut that opens a new tab and drops into interactive `find-file`, this is the workflow I like the most for staying in the same frame but opening up another project or file.

### Eshell
So I've used `shell` and `term` but they are awkward, and if I'm going to use a terminal I will just use a terminal. If I'm staying in emacs, then I'll use all the elisp features built into eshell.

I like VSCode's `Ctrl-backtick` shortcut to open a little shell window below the current file. I emulated that in my config. I also made a global eshell tab you can jump to using `<f4>`, that's the key I mapped on macOS to bring up iTerm. Both these two shortcuts toggle, so you can hit them twice to do a noop. 

This is where I keep some of my config files, and some emacs lisp files.

## Clojure shortcuts
### get the `lein test :only` command for the `deftest` at point

```emacs-lisp
(defun cider-current-deftest ()
  "Get the name of the deftest at point"
  (save-excursion
    (end-of-defun)
    (beginning-of-defun)
    (when (re-search-forward "(deftest \\([^ \n\t]+\\)" (point-at-eol) t)
      (let ((test-name (substring-no-properties (match-string 1))))
        (message test-name)
        test-name))))

(defun lein-test-only-command ()
  "Get the 'lein test :only $NAMESPACE/$TESTNAME' command"
  (interactive)
  (let ((ns (substring-no-properties (cider-current-ns)))
        (name (cider-current-deftest)))
    (let ((cmd (concat "lein test :only " ns "/" name)))
      (message cmd)
      cmd)))
```

## law: link types in org-mode
When using the spacemacs.el file, a new link type will be available in org-mode.

For example: typing [law:ORS/93.850](http://www.oregonlaws.org/ors/93.850) will 
automatically link it to the relevant page on oregonlaws.org.

Typing [law:USC/12/2605](https://www.law.cornell.edu/uscode/text/12/2605) will 
automatically link to the relevant page on cornell.edu's copy of the U.S. Code.

This is useful when taking notes that reference laws. I recently bought a house 
and it's hard to get through a single page without some specific law being referenced.


## common-words.el

This is my first [major mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/Major-Modes.html) for emacs. 
It was inspired by Randall Munroe's [Thing Explainer](https://xkcd.com/thing-explainer/) and Morten Just's [editor that 
doomed humanity](https://medium.com/@mortenjust/i-doomed-mankind-with-a-free-text-editor-ba6003319681#.utnh5bpjh).

The concept is simple, restrict your vocabulary to the 1000 most common words. If you can explain something using this 
reduced vocabulary, then you really understand the topic. This is a decent test of understanding because it's easy to 
learn a word, and even use that word in the right context, but still have no idea how it relates to other things.

This is not to say that specialized vocabulary is bad, it's quite useful for communicating between people with a lot of 
shared context. But when explaining things to people without that context, it's important to be able to expand jargon words
into common words, even if it results in a larger, slightly less accurate version.

Here is an animation of the editor mode in action:

![common-word-mode](common-word-mode.gif)
