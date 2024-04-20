#!/usr/bin/env emacs --script

;; Somehow Emacs GUI does not run as arm64 on macos so in order to install these
;; tree-sitter library we need to build them from a terminal where the
;; architecure is setup as arm64.

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")))

(mapc #'treesit-install-language-grammar
      (mapcar #'car treesit-language-source-alist))
