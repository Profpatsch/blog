---
title: Ligature Emulation in Emacs
subtitle: It’s not pretty, but the results are
date: 2017-05-04
tags:
  - emacs
---

Monday was (yet another)
[NixOS hackathon][hackathon] at [OpenLab Augsburg][ola].
[Maximilian][mhuber] was there and to my amazement
he got working ligatures in his Haskell files in Emacs! Ever since Hasklig
updated its format to use ligatures and private Unicode code points a while ago,
the hack I had used in my config stopped working.

Encouraged by that I decided to take a look on Tuesday. Long story short, I was
able to [get it working in a pretty satisfying way][done].

[hackathon]: https://www.meetup.com/Munich-NixOS-Meetup/events/239077247/
[mhuber]: https://github.com/maximilianhuber
[ola]: https://openlab-augsburg.de
[done]: https://github.com/i-tu/Hasklig/issues/84#issuecomment-298803495

What’s left to do is package it into a module and push to melpa.


### elisp still sucks, but it’s bearable, sometimes

I’m the kind of person who, when trying to fix something elisp related, normally
gives up two hours later and three macro calls deep. Yes, homoiconic,
non-lexically-scoped, self-rewriting code is not exactly my fetish.
This time the task and the library (`prettify-symbols-mode`) were simple enough
for that to not happen.

Some interesting technical trivia:

- elisp literal character syntax is `?c`. `?\t` is the tab character
- You join characters by `(string c1 c2 c3 ...)`
- [dash.el][dash] is pretty awesome and does what a functional programmer
  expects. Also, Rainbow Dash.
- Hasklig and FiraCode multi-column symbols actually [only occupy one column, on
  the far right of the glyph][glyph]. `my-correct-symbol-bounds` fixes emacs’
  rendering in that case.


[dash]: https://github.com/magnars/dash.el
[glyph]: https://github.com/tonsky/FiraCode/issues/211#issuecomment-239082368


## Appendix A

For reference, here’s the complete code as it stands now. Feel free to paste
into your config; let’s make it [MIT][mit]. Maybe link to this site, in case there are
updates.

[mit]: https://opensource.org/licenses/MIT

```elisp
 (defun my-correct-symbol-bounds (pretty-alist)
    "Prepend a TAB character to each symbol in this alist,
this way compose-region called by prettify-symbols-mode
will use the correct width of the symbols
instead of the width measured by char-width."
    (mapcar (lambda (el)
              (setcdr el (string ?\t (cdr el)))
              el)
            pretty-alist))

  (defun my-ligature-list (ligatures codepoint-start)
    "Create an alist of strings to replace with
codepoints starting from codepoint-start."
    (let ((codepoints (-iterate '1+ codepoint-start (length ligatures))))
      (-zip-pair ligatures codepoints)))

  ; list can be found at https://github.com/i-tu/Hasklig/blob/master/GlyphOrderAndAliasDB#L1588
  (setq my-hasklig-ligatures
    (let* ((ligs '("&&" "***" "*>" "\\\\" "||" "|>" "::"
                   "==" "===" "==>" "=>" "=<<" "!!" ">>"
                   ">>=" ">>>" ">>-" ">-" "->" "-<" "-<<"
                   "<*" "<*>" "<|" "<|>" "<$>" "<>" "<-"
                   "<<" "<<<" "<+>" ".." "..." "++" "+++"
                   "/=" ":::" ">=>" "->>" "<=>" "<=<" "<->")))
      (my-correct-symbol-bounds (my-ligature-list ligs #Xe100))))

  ;; nice glyphs for haskell with hasklig
  (defun my-set-hasklig-ligatures ()
    "Add hasklig ligatures for use with prettify-symbols-mode."
    (setq prettify-symbols-alist
          (append my-hasklig-ligatures prettify-symbols-alist))
    (prettify-symbols-mode))

  (add-hook 'haskell-mode-hook 'my-set-hasklig-ligatures)
```

## Appendix B (Update 1): FiraCode integration

I also created a mapping for [FiraCode][fira]. You need to grab the [additional
symbol font][symbol] that adds (most) ligatures to the unicode private use area.
Consult your system documentation on how to add it to your font cache.
Next add `"Fira Code"` and `"Fira Code Symbol"` to your font preferences. Symbol
only contains the additional characters, so you need both.

If you are on NixOS, the font package should be on the main branch shortly, [I
added a package][symbol-pkg].

[fira]: https://github.com/tonsky/FiraCode/
[symbol]: https://github.com/tonsky/FiraCode/issues/211#issuecomment-239058632
[symbol-pkg]: https://github.com/NixOS/nixpkgs/pull/25517

Here’s the mapping adjusted for FiraCode:

```elisp
  (setq my-fira-code-ligatures
    (let* ((ligs '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
                  "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
                  "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
                  "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
                  ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
                  "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
                  "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
                  "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
                  ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
                  "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
                  "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
                  "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
                  "x" ":" "+" "+" "*")))
      (my-correct-symbol-bounds (my-ligature-list ligs #Xe100))))
```
