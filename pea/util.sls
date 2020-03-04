(library (pea util)
  (export
    arg
    command
    define-enum
    my
    reverse-map
    slurp
    string-join string-trim-both
    write-now
    )
  (import
    (rnrs)
    (irregex)
    (only (chezscheme) datum))

  ;; [proc] arg: get first argument.
  ;; HMMM check return type is singleton?
  (define arg
    (lambda (input)
      (cadr input)))

  ;; [proc] command: sanitise 1st arg (if any) of input.
  (define command
    (lambda (input)
      (cond
        [(null? input)
         'EMPTY]
        [(list? input)
         (car input)]
        [else
          input])))

  ;; [syntax] define-enum: generates a syntax transformer that evaluates the value of an enum at compile time.
  ;; eg, using trace-define-syntax:
  ;; > (define-enum e [a 1] [b 2] [c 3])
  ;; |(define-enum (define-enum e (a 1) (b 2) (c 3)))
  ;; |(define-syntax e
  ;;    (lambda (x)
  ;;      (syntax-case x ()
  ;;        [(_ v) (eq? (datum v) (syntax->datum #'a)) #'1]
  ;;        [(_ v) (eq? (datum v) (syntax->datum #'b)) #'2]
  ;;        [(_ v) (eq? (datum v) (syntax->datum #'c)) #'3])))
  ;; > (e a)
  ;; 1
  ;; > (e b)
  ;; 2
  ;; > (e c)
  ;; 3
  ;; > (e d)
  ;; Exception: invalid syntax (e d)
  ;; Type (debug) to enter the debugger.
  ;; >
  (define-syntax define-enum
    (syntax-rules ()
      [(_ group (var* val*) ...)
       (define-syntax group
         (lambda (x)
           (syntax-case x ()
             [(_ v)
              (eq? (datum v) (syntax->datum #'var*))
              #'val*] ...)))]))

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

  ;; [proc] write-now: write message and flush port.
  ;; If I had a dollar for the number of times i've forgotten to flush after write....
  (define write-now
    (lambda (msg port)
      (write msg port)
      ;; NB newline is very import here. Chez scheme reader (read) seems to require it.
      ;; Otherwise pea ends up with the reader in weird states waiting for more data.
      (newline port)
      (flush-output-port port)))
  )
