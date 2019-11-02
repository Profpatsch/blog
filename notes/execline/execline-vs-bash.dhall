-- TODO: loops, parameter handling, heredocs, string-munging
  λ(markdownToHtml : Text → Text)
→ let Prelude =
        ./imports/Prelude/package.dhall

  let el =
          λ(bash : Text)
        → λ(execline : Text)
        → { bash = bash, execline = execline, note = None Text }

  let BashVsDhall = { bash : Text, execline : Text, note : Optional Text }

  let Final = < Markdown : Text | Html : Text >

  let list
      : List BashVsDhall
      = [ el
          "a; b"
          "foreground { a } b"
        , el "if a; then b; fi" "if { a } b"
        , el "if a; then b; fi; c" "foreground { if { a } b } c"
        , el "a && b" "if { a } b"
        ,   el "a || b" "ifelse { a } { } b"
          ⫽ { note = Some "`{}` is wrong, `{ }` is right" }
        ,   el
            "a && b || c"
            "ifelse { a } { b } c"
          ⫽ { note =
                Some
                "This is `if`/`then`/`else`; `ifthenelse` is for continuing after the branches finish"
            }
        , el "if a; then b; else c; fi" "ifelse { a } { b } c"
        , el "a && b || c; d" "ifthenelse { a } { b } { c } d"
        , el "if a; then b; else c; fi; d" "ifthenelse { a } { b } { c } d"
        , el "a &; b" "background { a } b"
        , el "a | b | c" "pipeline { a } pipeline { b } c"
        , el "a > f" "redirfd -w 1 f a"
        , el "a >> f" "redirfd -a 1 f a"
        ,   el "a < f" "redirfd -r 0 f a"
          ⫽ { note =
                Some "redirfd is very powerful, see help page for more info"
            }
        ,   el "a >&2" "fdmove -c 1 2 a"
          ⫽ { note = Some "`fdmove 1 2` without `-c` closes 2" }
        ]

  let optText =
          λ(opt : Optional Text)
        → Optional/fold Text opt Text (λ(t : Text) → t) ""

  let toFinal
      : List BashVsDhall → List Final
      =   λ(list : List BashVsDhall)
        → let element =
                  λ(x : BashVsDhall)
                → Final.Html
                  ''
                  <tr>
                    <td><pre><code>${x.bash}</pre></code></td>
                    <td><pre><code>${x.execline}</pre></code></td>
                    <td>${markdownToHtml (optText x.note)}</td>
                  </tr>
                  ''

          in    [ Final.Markdown "ehlo dis is template\n"
                , Final.Html
                  ''
                  <table>
                    <tr>
                      <td>bash</td>
                      <td>execline</td>
                      <td>note</td>
                    </tr>
                  ''
                ]
              # Prelude.List.map BashVsDhall Final element list
              # [ Final.Html "</table>\n" ]

  in  toFinal list
