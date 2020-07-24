(library (pea util)
  (export
    arg
    command
    condition->doh
    define-enum
    list-slice
    my
    object->string
    (rename (safe-input-port-ready? input-port-ready?))
    read-trim-right
    reverse-map
    safe-substring
    slurp
    string-join string-startswith? string-trim-both
    write-now
    )
  (import
    (rnrs)
    (irregex)
    (only (chezscheme) datum display-condition input-port-ready? list-head))

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

  ;; [proc] condition->doh: converts the display-condition message to a DOH message.
  ;; [return] DOH message object.
  (define condition->doh
    (lambda (e msg)
      (let-values ([(port getter) (open-string-output-port)])
        (display-condition e port)
        `(DOH ,msg ,(getter)))))

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

  ;; [proc] list-slice: return exact slice or as much as able.
  (define list-slice
    (lambda (lst offset len)
      (let ([lst-len (length lst)])
        (cond
          [(<= (+ offset len) lst-len)
           (list-head (list-tail lst offset) len)]
          [(> offset lst-len)
           '()]
          [else
            (list-tail lst offset)]))))

  ;; [syntax] my: short-hand for batch defines.
  ;; Name gratuitously taken from perl. I also like that it's nice and short.
  (define-syntax my
    (syntax-rules ()
      [(_ (name val) ...)
       (begin
         (define name val) ...)]))

  ;; [proc] object->string: returns the scheme object in string form.
  (define object->string
    (lambda (obj)
      (call-with-string-output-port
        (lambda (p)
          (write obj p)))))

  ;; Wraps input-port-ready? so that any raised exception is converted to false.
  ;; The custom port for the socket is raising &i/o-read-error occasionally.
  ;; See: chez s/io.ss ~= /cannot determine ready status/
  ;; It appears to trigger when port is empty and has not reached eof.
  (define safe-input-port-ready?
    (lambda (port)
      (guard (e [else #f])
        (input-port-ready? port))))

  ;; [proc] read-trim-right: performs a read and then trims trailing whitespace.
  (define read-trim-right
    (case-lambda
      [()
       (read-trim-right (current-input-port))]
      [(port)
       (let ([ret (guard (e [else (eof-object)])
                    ;; (read) can exception if it encounters a scheme object it doesn't know how to instantiate.
                    ;; Return EOF, clients case use that to trigger a port close.
                    (read port))])
         ;; Datum is read into 'ret'. Remove any further whitespace.
         (let loop ()
           (cond
             [(safe-input-port-ready? port)
              (let ([ch (peek-char port)])
                (cond
                  ;; NB char-whitespace? will exception if it sees an eof-object.
                  [(eof-object? ch)
                   ret]
                  [(char-whitespace? ch)
                   (read-char port)
                   (loop)]))]
             [else
               ret])))]))

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

  ;; [proc] safe-substring: as substring but returns #f on error rather than exception.
  (define safe-substring
    (lambda (str start end)
      (guard (e [else str])
        (substring str start end))))

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

  ;; [proc] string-startswith?: tests whether str starts with prefix.
  ;; This is a quick and dirty (read inefficient) version that uses irregex.
  (define string-startswith?
    (lambda (str prefix)
      (irregex-match-data? (irregex-search `(w/case bos ,prefix) str))))

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
