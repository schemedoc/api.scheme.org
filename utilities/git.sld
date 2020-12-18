(define-library (utilities git)
  (export git-clone-or-fetch
          git-show
          git-ls-tree
          git-rev-parse)
  (import (scheme base)
          (scheme file)
          (utilities run))
  (begin

    (define (git-clone-or-fetch url directory)
      (unless (file-exists? directory)
        (run "git" "clone" "--bare" "-depth" "1" "--" url directory))
      (run-in-directory directory "git" "fetch" "--all" "--tags" "--prune"))

    (define (git-show ref)
      (run/output-bytevector "git" "show" ref))

    (define (git-ls-tree ref)
      (run/output-string "git" "ls-tree" ref))

    (define (git-rev-parse ref)
      (run/output-line "git" "rev-parse" ref))))
