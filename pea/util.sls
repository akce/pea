(library (pea util)
  (export
    my
    reverse-map
    slurp
    string-join string-trim-both
    )
  (import
    (rnrs)
    (irregex))

  ;; [syntax] my: short-hand for batch defines.
  ;; Name gratuitously taken from perl. I also like that it's nice and short.
  (define-syntax my
    (syntax-rules ()
      [(_ (name val) ...)
       (begin
         (define name val) ...)]))

  ;; [proc] reverse-map: like map, but list is returned in reverse.
  ;; TODO support multiple lists.
  (define reverse-map
    (lambda (proc lst)
      (let loop ([ls lst] [acc '()])
        (cond
          [(null? ls)
           acc]
          [else
            (loop (cdr ls) (cons (proc (car ls)) acc))]))))

  ;; [proc] slurp: Read all lines from a text file.
  ;; Name is akin to the perl function.
  ;; All lines of a file are returned as a list with newlines removed.
  (define slurp
    (lambda (path)
      (let ([f (open-file-input-port
                 path
                 (file-options no-create)
                 (buffer-mode line)
                 (make-transcoder (utf-8-codec)))])
        (let loop ([line (get-line f)] [lines '()])
          (cond
            [(eof-object? line)
             (close-input-port f)
             (reverse lines)]
            [else
              (loop (get-line f) (cons line lines))])))))

  ;; [proc] string-join: join all string parts together using separator.
  ;;
  ;; Note that the signature to this version of string-join differs to that found in SRFI-13.
  ;; The separator is the first arg and therefore always explicit which allows for the string
  ;; parts as regular arguments, rather than a list of strings.
  ;;
  ;; Naive implementation that uses (potentially) multiple calls to string-append.
  ;; TODO use alternate name to differentiate from SRFI-13?
  (define string-join
    (lambda (sep . str-parts)
      (cond
        [(null? str-parts)
         ""]
        [else
          (let loop ([acc (car str-parts)] [rest (cdr str-parts)])
            (cond
              [(null? rest)
               acc]
              [else
                (loop (string-append acc sep (car rest)) (cdr rest))]))])))

  ;; [proc] string-trim-both: kind of the same as that found in (srfi :152 strings)
  ;; Defined using irregex and only supports whitespace trimming.
  (define string-trim-both
    (let ([p (irregex '(w/nocase (* space ) (submatch (* nonl)) (* space)))])
      (lambda (line)
        (irregex-match-substring (irregex-search p line) 1))))
  )
