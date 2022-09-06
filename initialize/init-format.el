(use-package format-all
  :straight (:host github :repo "lassik/emacs-format-all-the-code")
  :config
  (setq format-all-default-formatters '(("Assembly" asmfmt)
					("ATS" atsfmt)
					("Bazel" buildifier)
					("BibTeX" emacs-bibtex)
					("C" clang-format)
					("C#" clang-format)
					("C++" clang-format)
					("Cabal Config" cabal-fmt)
					("Clojure" zprint)
					("CMake" cmake-format)
					("Crystal" crystal)
					("CSS" prettier)
					("Cuda" clang-format)
					("D" dfmt)
					("Dart" dart-format)
					("Dhall" dhall)
					("Dockerfile" dockfmt)
					("Elixir" mix-format)
					("Elm" elm-format)
					("Emacs Lisp" emacs-lisp)
					("Erlang" efmt)
					("F#" fantomas)
					("Fish" fish-indent)
					("Fortran Free Form" fprettify)
					("GLSL" clang-format)
					("Go" gofmt)
					("GraphQL" prettier)
					("Haskell" brittany)
					("HTML" html-tidy)
					("HTML+ERB" erb-format)
					("Java" clang-format)
					("JavaScript" prettier)
					("JSON" prettier)
					("JSON5" prettier)
					("Jsonnet" jsonnetfmt)
					("JSX" prettier)
					("Kotlin" ktlint)
					("LaTeX" latexindent)
					("Less" prettier)
					("Literate Haskell" brittany)
					("Lua" lua-fmt)
					("Markdown" prettier)
					("Nix" nixpkgs-fmt)
					("Objective-C" clang-format)
					("OCaml" ocp-indent)
					("Perl" perltidy)
					("PHP" prettier)
					("Protocol Buffer" clang-format)
					("PureScript" purty)
					("Python" yapf)
					("R" styler)
					("Reason" bsrefmt)
					("ReScript" rescript)
					("Ruby" rufo)
					("Rust" rustfmt)
					("Scala" scalafmt)
					("SCSS" prettier)
					("Shell" shfmt)
					("Solidity" prettier)
					("SQL" sqlformat)
					("Svelte" prettier)
					("Swift" swiftformat)
					("Terraform" terraform-fmt)
					("TOML" prettier)
					("TSX" prettier)
					("TypeScript" prettier)
					("V" v-fmt)
					("Verilog" istyle-verilog)
					("Vue" prettier)
					("XML" html-tidy)
					("YAML" prettier)
					("Zig" zig)
					("_Angular" prettier)
					("_Flow" prettier)
					("_Gleam" gleam)
					("_Ledger" ledger-mode)
					("_Nginx" nginxfmt)
					("_Snakemake" snakefmt))))

(provide 'init-format)
