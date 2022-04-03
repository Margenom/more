;For start: csc .all.scm; ln -s .all new; ln -s .all add2; ln -s .all last2
(import 
	(chicken io) (chicken file) (chicken sort) (chicken time) (chicken file)
	(chicken string) (chicken irregex) (chicken time posix) (chicken process-context) 
  colorize spiffy intarweb uri-common base64
	(chicken process) srfi-1 utf8)

(define (home #!optional file) 
	(let ((base (string-append (get-environment-variable "HOME") "/me")))
		(if file (string-append base "/" (->string file)) base))) 
(define (isdiary? str) (let ((num (string->number str))) (and num (> num 1e9))))
(define (last-rec len) (take (sort (filter isdiary? (directory (home))) 
	(lambda(a b) (> (string->number a) (string->number b)))) len))
(define (append2file line file) (with-output-to-file file (lambda() (print line)) #:append))
(define (rmpath file)  (irregex-replace "(.*/)*" file ""))

;commands
(define comms '())
(define (appcomm name comm) (set! comms (cons (cons name comm) comms))) 

(define (new-rec) 
	(process-execute (get-environment-variable "EDITOR") (list (home (current-seconds)))))
(appcomm "more" new-rec) 
(define (app-file2 cat file) 
	(let ((patha (string-append (current-directory) "/" file)) (pathb (home (rmpath file))))
		(unless (string=? patha pathb) (copy-file patha pathb)))
	(append2file (rmpath file) (home cat)))
(appcomm "col" app-file2) 
(define (app-last2 cat) (append2file (car (last-rec 1)) (home cat)))
(appcomm "last" app-last2)

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
(define (read-blob file) (base64-encode 
	(with-input-from-file file (lambda() (read-string #f)))))
(define (read-file name) (with-input-from-file name read-lines)) 
(define (lines2str lns) (fold (lambda(k v) (string-append v k "\n")) "\n" lns))
(define (file2html name) (let*((data (lambda() (lines2str (read-file name))))
		(withhead (lambda(n c) (string-append "<p><b><i>" n "</i></b></p><pre>" c "</pre>")))
		(withlink (lambda(n a) (string-append "<a href=\"" a "\">" n "</a>")))
		(coloradd (lambda(st) (withhead name (string-append "<pre>" (html-colorize st (data)) "</pre>"))))
		(spoiler (lambda(n c) (string-append "<details><summary>" n "</summary>" c "</details>")))
		(subs (irregex-match "([^.]*)(\\.(.*))?" name))
		(fname (irregex-match-substring subs 1))
		(ftype (irregex-match-substring subs 3)))
	(if ftype (let ((asym (string->symbol ftype)))
		(cond 
			((member asym '(jpg png gif jpeg)) 
				(string-append "<img src=\"data:"
					(symbol->string (file-extension->mime-type ftype)) ";base64,"
					(read-blob name) "\" alt'\"" name "\">"))
			((eq? asym 'scm) (coloradd 'scheme))
			((coloring-type-exists? asym) (coloradd asym))
			(else (spoiler name (string-append "<pre><code>" (data) "</code></pre>")))))
		(let ((urlrex  "(https?|ftp)://[-0-9a-zA-Z._+~#=@:]{1,256}[^\t\n]*"))
			(if (isdiary? name) 
				(withhead (seconds->string (string->number name)) 
					(irregex-replace/all urlrex (data) "<a href=\"" 0 "\">" 0 "</a>"))
				(spoiler (string-append "Category " (withlink name name)) (string-append "<pre>" (data) "</pre>")))))))
(define (view-file-list flst #!optional dop)
	(if (null? flst) '()
		(cons (if (file-exists? (car flst)) (cons (car flst) ((if dop dop file2html) (car flst)))
				(string-append "<p>" (car flst) "</p>"))
			(view-file-list (cdr flst)))))
(define (bstrcmp? a b) (positive? (string-compare3 a b)))
(define (isspec? a) (irregex-match "[^.]+\\..+" a))
(define (iscat? a) (not (or (isspec? a) (isdiary? a))))

(define (web-server) 
	(print "Starting server...")
	(change-directory (home))
	(define (handle-greeting continue)
  	(let*((uri (request-uri (current-request)))
					(dir (directory))
					(header (lambda(n) (string-append "<head><meta charset=\"utf8\"><title>" n "</title></head>" styles)))
					(cats (filter (lambda(k) (and (iscat? k) (not (member k (map car comms))))) dir))
					(diary (reverse (sort (filter isdiary? dir) bstrcmp?)))
					(spec (reverse (sort (filter isspec? dir) bstrcmp?)))
					(view2str (lambda(k) (apply string-append (map cdr (view-file-list k)))))
					(path (uri-path uri)))
			(print (request-headers (current-request)))
			(cond ((equal? path '(/ "")) (send-response status: 'ok body: 
				(string-append (header "ALL") (view2str cats) "<hr>" (view2str diary) "<hr>" (view2str spec))))
				((and (> (length path) 1) (member (cadr path) cats)) (send-response status: 'ok body: 
					(string-append (header (cadr path)) (view2str (read-file (cadr path)))))) 
				(else (continue)))))
	(vhost-map `((".*" . ,handle-greeting))) (start-server))

;main
(define (main)
	(define Com (assoc (cadr (argv)) comms))
	(if Com (apply (cdr Com) (cddr (argv))) (web-server)))

(main)
