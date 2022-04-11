;For start: csc .all.scm; ln -s .all new; ln -s .all add2; ln -s .all last2
(import 
	(chicken io) (chicken file) (chicken sort) (chicken time) (chicken file)
	(chicken string) (chicken irregex) (chicken time posix) (chicken process-context) 
  colorize spiffy intarweb uri-common base64
	(chicken process) srfi-1 utf8)
;web veiewer with colors and hiper links for other devices (like phone) in local network
(define styles #<<CSS
<style>
.comment 			{color: #aaeeaa;}
.character 		{color: #8400ff;}
.string 			{color: #d8e30e;}
.variable 		{color: #3449d1;}
.function 		{color: #29cf13;}
.attribute 		{color: #8400ff;}
.macro 				{color: #3449d1;}
.atom 				{color: #d8e30e;}
.special 			{color: #29cf13;}
.symbol 			{color: #c70031;}
.keyword 			{color: #0798ab;}
.paren1 			{color: #f01578;}
.paren2 			{color: #6ce05c;}
.paren3 			{color: #f3f79e;}
.paren4 			{color: #97a4f7;}
.paren5 			{color: #c495f7;}
.paren6 			{color: #68f2e0;}
.default 			{color: #e2d1e3;}
.syntax-error {color: #c70031;}
.diff-normal 	{color: #e2d1e3;}
.diff-added 	{color: #d8e30e;}
.diff-deleted {color: #c70031;}
body {color: #aaaaaa; background-color: #0e100a;}
</style>
CSS
)
;commands
(define comms '())
(define (appcomm name comm) (set! comms (cons (cons name comm) comms)))
(define (home #!optional file) (let ((base (string-append (get-environment-variable "HOME") "/me")))
		(if file (string-append base "/" (->string file)) base)))
(define (isdiary? str) (let ((num (string->number str))) (and num (> num 1e9))))
; More - text only consist from data what be in my mind when i write more about this
(appcomm "more" (lambda()
	(process-execute (get-environment-variable "EDITOR") (list (home (current-seconds))))))
(define (append2file line file) (with-output-to-file file (lambda() (print line)) #:append))
(define (rmpath file)  (irregex-replace "(.*/)*" file ""))
(appcomm "col" (lambda (cat file)
	(let ((patha (string-append (current-directory) "/" file)) (pathb (home (rmpath file))))
		(unless (string=? patha pathb) (copy-file patha pathb)))
	(append2file (rmpath file) (home cat))))
(define (last-rec len) (take (sort (filter isdiary? (directory (home)))
	(lambda(a b) (> (string->number a) (string->number b)))) len))
(appcomm "last" app-last2) (lambda (cat) (append2file (car (last-rec 1)) (home cat)))
(define (read-file file) (with-input-from-file file (lambda() (read-string #f))))
(define (string-lines Str) (string-split Str "\n"))
(define (collected #!optional List) (let*( (Etc (if List List (directory (home))))
		(col-filter (lambda (R) (call-with-values (lambda() (partition R Etc))
				(lambda(M O) (set! Etc O) M))))
		(rx-filter (lambda (R) (col-filter (lambda(K) (irregexp-match? R K)))))
		(Dir (col-filter (lambda(K) (directory? (home K)))))
		(More (rx-filter "[0-9]{10,12}"))
		(Cat (rx-filter  "[a-zA-ZА-Яа-я0-9]+"))
		(Spec (rx-filter "[a-zA-ZА-Яа-я0-9]+\\.[a-zA-ZА-Яа-я0-9]+")))
	(map (lambda(K) (or K '())) (list Cat More Spec Dir Etc))))
(define (more-header-to-human More) (seconds->string (string->number More)))
(define (more2web More) (format #f "<p><b><i>~a</i></b></p><pre>~a</pre>"
	(more-header-to-human More) (irregex-replace/all "(https?|ftp)://[-0-9a-zA-Z._+~#=@:]{1,256}[^\t\n]*"
		(htmlize (read-file (home More))) "<a href=\"" 0 "\">" 0 "</a>")))
(define SYS_MIME (append (map (lambda(Str (string-split Str " "))) (string-lines (read-file "/etc/mime.types")))
	'(("text/x-scheme" "scm" "sps" "sls" "sld" "ss"))))
(define (system-mime Expr) (car (or (member Expr SYS_MIME member)'(#f))))
(define (spec2web Spec) (let* ((Expr (take-right (string-split Spec ".") 1))
		(Mime (system-mime Expr))
		(Type (and Mime (car (string-split (car Mime) "/"))))
		(Ctype (and Mime (string->symbol (take-right (string-split (car Mime) "-") 1)))) )
	(case Type (("image") (format #f "<img src=~s alt=~s>" (format #f "/res/~a" Spec) Spec))
		(("text") (format #f "<pre><i><b>Name: ~a</b></i><code>~a</code></pre>" Spec ((lambda(T)
			(if (coloring-type-exists? Ctype) (html-colorize Ctype T) T)) (read-file (home Spec)))))
		(else (format #f "<a href=~s>~a</a>" (format #f "/res/~a" Spec) Spec)))))
(define (cat2web Cat) (format #f "<details><summary>Category: <a href=~s>~a</a></summary><pre>~a</pre></details>"
	(format #f "/cat/~a" Cat) Cat (read-file (home Cat))))
(define (serv-page #!optional Cat) (define List (collected Cat)) (string-append
	(format #f "<head><meta charset='utf8'><title>~a</title></head>" (or Cat "All"))
	(apply string-append (apply append (map (lambda(L K) (map L K)) (list cat2web more2web spec2web) (take 3))))))
(define (serv-start)
	(define (main-handle cont) (send-response status: 'ok body: (serv-page)))
	(define (res-handle cont)cont)
	(define (cat-handle cont) (define Cat ((lambda(P) (and (> (length P) 2) (caddr P)))
			(uri-path (request-uri (current-request)))))
		(if Cat (send-response status: 'ok body: (serv-page (string-lines (read-file (home Cat))))) cont))
	(define handle cont (define Path (uri-path (request-uri (current-request))))
		(case (take Path 2) (((/ "res")) (res-handle cont))
			(((/ "cat")) (cat-handle cont))
			(else main-handle cont)))
	(vhost-map `((".*" . ,handle))) (start-server))
;main
(define (main)
	(define Com (assoc (cadr (argv)) comms))
	(if Com (apply (cdr Com) (cddr (argv))) (web-server)))
(main)
