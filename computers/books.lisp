(defmacro html (&body body)
  `(with-open-file (*standard-output* "/home/lars/html/computers/books.html"
		    :direction :output
		    :if-exists :supersede
		    :external-format :latin-1)
    (write-line "<html>")
    ,@body
    (write-line "</html>")))

(macrolet ((defhtml (name &optional no-endtag-p)
	       `(defmacro ,name (&body body)
		  (let ((attributes nil))
		    (do ()
			((not (keywordp (first body))))
		      (setf attributes
			    `(,@attributes
			      (write-string
			       ,(format nil " ~A=\"~A\""
					(first body) (second body)))))
		      (setf body (cddr body)))
		    (setf body (mapcar (lambda (form)
					 (if (stringp form)
					     `(write-string ,form)
					     form))
				       body))
		    `(progn
		      (write-string ,',(format nil "<~A" name))
		      ,@attributes
		      (write-string ">")
		      ,@body
		      ,@',(unless no-endtag-p
			    `((write-string ,(format nil "</~A>" name)))))))))
  (defhtml head)
  (defhtml title)
  (defhtml body)
  (defhtml table)
  (defhtml tr)
  (defhtml td)
  (defhtml h1)
  (defhtml h2)
  (defhtml center)
  (defhtml s)
  (defhtml b)
  (defhtml a)
  (defhtml p)
  (defhtml ul)
  (defhtml li)
  (defhtml br :no-endtag)
  (defhtml img :no-endtag))

(defmacro book-list (title &body list)
  (do ((result `(progn (tr (td :colspan 3 (h2 (center ,title))))))
       (books list (cddr books)))
      ((null books)
       result)
    (let ((left (first books))
	  (right (second books)))
      (when (getf right :image)
	(setf left `(,@left :image2 ,(getf right :image)))
	(remf right :image))
      (cond
	((getf right :alt)
	 (setf left `(,@left :alt2 ,(getf right :alt)))
	 (remf right :alt))
	(t
	 (setf left `(,@left :alt2 ,(getf right :title)))))
      (setf result `(,@result
		     (book-left ,@left)
		     ,(if right
			  `(book-right ,@right)
			  '(tr)))))))

(defun nth-string (n)
  (check-type n (integer 0 *))
  (format nil "~D~A" n
	  (if (eql (mod (truncate n 10) 10) 1)
	      "th"
	      (case (mod n 10)
		(1 "st") (2 "nd") (3 "rd") (t "th")))))

(defun book (title edition link author author-note year isbn note)
  (when (stringp title)
    (setf title (list title)))
  (when (stringp author)
    (setf author (list author)))
  `(,@(if link		`((a :href ,link ,@title))
			title)
    ,@(when edition	(list (format nil ", ~A ed"
				      (if (stringp edition)
					  edition
					  (nth-string edition)))))
    (br)
    ,@			author
    ,@(when author-note	(list " " author-note))
    ,@(when year	(list (format nil ", ~D" year)))
    ,@(when isbn	`((br) (isbn ,isbn)))
    ,@(when note	`((br) "[" ,note "]"))))

(defmacro with-gensyms ((&rest syms) &body body)
  `(let ,(mapcar #'list syms '#1=((gensym) . #1#))
    ,@body))

(defmacro do-plist ((key val plist &optional result) &body body)
  (with-gensyms (list)
    `(do ((,list ,plist (cddr ,list))
	  ,key ,val)
         ((null ,list)
	  ,result)
      (setq ,key (first ,list) ,val (second ,list))
      ,@body)))

(defun author-html (author)
  (let ((x (assoc author *author-link* :test #'string=)))
    (if x
	`(a :href ,(cdr x) ,author)
	author)))  

(defun extract-authors (keys)
  (let ((authors nil))
    (do-plist (key val keys)
      (when (eq key :author)
	(push val authors)))

    (loop for (author next . rest) on (nreverse authors)
	  collect (if (stringp author)
		      (author-html author)
		      author)
	  when next collect (if rest ", " " &amp; "))))

(defmacro book-left (&rest keys
		     &key title edition link author author-note year isbn
		     image (alt title) image2 alt2 note)
  (setf author (extract-authors keys))
  `(tr (td :align "right" :valign "top" :rowspan 2
	,@(when image
		`((img :src ,(format nil "books/~A.jpg" image)
		   :title ,alt :alt ,alt))))
       (td :valign "top" ,@(book title edition link author author-note year
				 isbn note))
       (td :valign "bottom" :rowspan "2"
	,@(when image2
		`((img :src ,(format nil "books/~A.jpg" image2)
		   :title ,alt2 :alt ,alt2))))))

(defmacro book-right (&rest keys
		      &key title edition link author author-note year isbn
		      note)
  (setf author (extract-authors keys))
  `(tr (td :align "right" :valign "bottom"
	   ,@(book title edition link author author-note year isbn note))))

(defmacro isbn (isbn)
  `(progn
    (a :href ,(format nil "http://isbn.nu/~A" isbn) "ISBN")
    (write-string " ")
    (a :href ,(format nil "http://bokpris.com/~A" isbn) ,isbn)))

(defparameter *author-link*
  '(("A K Dewdney" .	"http://www.csd.uwo.ca/faculty/akd/")
    ("AMD" .		"http://www.amd.com/")
    ("Abraham Silberschatz" . "http://cs-www.cs.yale.edu/homes/avi/")
    ("Alfred V Aho" .	"http://www.cs.bell-labs.com/who/aho/")
    ("Anders Haraldsson" . "http://www.ida.liu.se/~andha/")
    ("Andreas Paepcke" . "http://www-diglib.stanford.edu/~paepcke/")
    ("Andrew Glassner" . "http://www.glassner.com/")
    ("Berthold Klaus Paul Horn" . "http://www.ai.mit.edu/people/bkph/bkph.html")
    ("Brian W Kernighan" . "http://www.cs.bell-labs.com/who/bwk/")
    ("Bruce Schneier" .	"http://www.schneier.com/")
    ("Christian Queinnec" . "http://www-spi.lip6.fr/~queinnec/WWW/Queinnec.html")
    ("Daniel G Bobrow" . "http://www2.parc.com/spl/members/bobrow/")
    ("Daniel P Friedman" . "http://www.cs.indiana.edu/~dfried/")
    ("David Flanagan" .	"http://www.davidflanagan.com/")
    ("Dennis Ritchie" .	"http://www.cs.bell-labs.com/who/dmr/")
    ("Douglas Hofstadter" . "http://www.cogs.indiana.edu/people/homepages/hofstadter.html")
    ("Eric S Raymond" .	"http://catb.org/~esr/")
    ("Franz, Inc" .	"http://www.franz.com/")
    ("Gerald Sussman" .	"http://www.swiss.ai.mit.edu/~gjs/")
    ("Greg Humphreys" .	"http://www.cs.virginia.edu/~grh6v/")
    ("Gregor Kiczales" . "http://www.cs.ubc.ca/~gregor/")
    ("Gregory R Andrews" . "http://www.cs.arizona.edu/people/greg/")
    ("Guy Deutscher" .	"http://website.leidenuniv.nl/~deutscherg/")
    ("Harold Abelson" .	"http://www.swiss.ai.mit.edu/~hal/")
    ("James Gosling" .	"http://java.sun.com/people/jag/")
    ("Jan Skansholm" . "http://www.math.chalmers.se/~skanshol/")
    ("John K Ousterhout" . "http://home.pacbell.net/ouster/")
    ("John McCarthy" .	"http://www-formal.stanford.edu/jmc/")
    ("Kavita Bala" . "http://www.cs.cornell.edu/~kb/")
    ("Ken Arnold" .	"http://java.sun.com/people/arnold/")
    ("Lawrence C Paulson" . "http://www.cl.cam.ac.uk/users/lcp/")
    ("Leo Brodie" .	"http://home.earthlink.net/~lbrodie/")
    ("Matt Pharr" .	"http://pharr.org/matt/")
    ("Matthias Felleisen" . "http://www.ccs.neu.edu/home/matthias/")
    ("Motorola, Inc" .	"http://www.motorola.com/")
    ("Neal Stephenson" . "http://www.well.com/user/neal/")
    ("P J Plauger" .	"http://www.plauger.com/")
    ("Patrick Henry Winston" . "http://www.ai.mit.edu/people/phw/phw.html")
    ("Paul Graham" .	"http://www.paulgraham.com/")
    ("Peter Galvin" .	"http://www.petergalvin.org/")
    ("Peter Norvig" .	"http://www.norvig.com/")
    ("Peter Seibel" .	"http://javamonkey.com/")
    ("Peter van der Linden" . "http://pvdl.best.vwh.net/")
    ("Philip Dutré" . "http://www.cs.kuleuven.ac.be/~phil/")
    ("Richard Jones" .	"http://www.cs.kent.ac.uk/people/staff/rej/")
    ("Richard P Gabriel" . "http://www.dreamsongs.com/")
    ("Rob Pike" .	"http://herpolhode.com/rob/")
    ("Ronald A Olsson" . "http://www.cs.ucdavis.edu/~olsson/")
    ("Samuel P Harbison" . "http://www.carlow.edu/~sharbison/")
    ("Stephen Slade" .	"http://www.stern.nyu.edu/~sslade/")
    ("Stuart Russell" .	"http://www.cs.berkeley.edu/~russell/")
    ("Tucker Withington" . "http://pt.withy.org/")
    ("W Richard Stevens" . "http://www.kohala.com/start/")))

(html
  (head (title "Computer-related books"))
  (body
    (table
      (tr (td :colspan 3 (h1 (center "Computer-related books"))))

      (book-list "Programming in General"

       (:title	"Structure and Interpretation of Computer Programs"
        :edition 2
        :link	"http://mitpress.mit.edu/sicp/"
        :author	"Harold Abelson"
        :author	"Gerald Sussman"
        :year	1996
        :isbn	"0262510871"
        :image	"Structure_and_Interpretation_of_Computer_Programs")

       (:title "Programming Pearls"
        :edition 2
        :link "http://www.cs.bell-labs.com/cm/cs/pearls/"
        :author "Jon Bentley"
        :year 2000
        :isbn "0201657880"
        :image "Programming_Pearls_2e")

       (:title "Hacker's Delight"
        :link "http://www.hackersdelight.org/"
        :author "Henry S Warren"
        :year 2003
        :isbn "0201914654"
        :image "Hacker's_Delight"
	:note "See also http://graphics.stanford.edu/~seander/bithacks.html")

       (:title "The Practice of Programming"
        :link "http://cm.bell-labs.com/cm/cs/tpop/"
        :author "Brian W Kernighan"
        :author "Rob Pike"
        :year 1999
        :isbn "020161586X"
        :image "The_Practice_of_Programming")

       (:title "Programming on Purpose - Essays on Software Design"
        :author "P J Plauger"
        :year 1993
        :isbn "0137213743"
        :image "Programming_on_Purpose")

       (:title "The New Turing Omnibus - 66 Excursions in Computer Sience"
        :link "http://www.csd.uwo.ca/faculty/akd/PERSONAL/books_and_articles.html#books"
        :author "A K Dewdney"
        :year 1993
        :isbn "0805071660"
        :image "The_New_Turing_Omnibus")

       (:title "The Elements of Programming Style"
        :edition 2
        :author "Brian W Kernighan"
        :author "P J Plauger"
        :year 1978
        :isbn "0070342075"
        :image "The_Elements_of_Programming_Style"))

      (book-list "Languages: C"

       (:title "The C Programming Language"
        :edition 2
        :link "http://cm.bell-labs.com/cm/cs/cbook/"
        :author "Brian W Kernighan"
        :author "Dennis Ritchie"
        :year 1988
        :isbn "0131103628"
        :image "The_C_Programming_Language_2e")

       (:title "C: A Reference Manual"
        :edition 5
        :link "http://www.careferencemanual.com/"
        :author "Samuel P Harbison"
        :author "Guy L Steele"
        :year 2002
        :isbn "013089592X"
        :image "C:_A_Reference_Manual_5e")

       (:title "Expert C Programming - Deep C Secrets"
        :link "http://pvdl.best.vwh.net/c/"
        :author "Peter van der Linden"
        :year 1994
        :isbn "0131774298"
        :image "Expert_C_Programming")

       (:title "The Standard C Library"
        :author "P J Plauger"
        :year 1992
        :isbn "0131315099"
        :image "The_Standard_C_Library")

       (:title "C: The Complete Reference"
        :edition 4
        :author "Herbert Schildt"
        :year 2000
        :isbn "0072121246"
        :image "C:_The_Complete_Reference_4e")

       (:title "Object-Oriented programming and the Objective C Language"
        :link "http://www.toodarkpark.org/computers/objc/"
        :author "NeXT Computer, Inc"
        :year 1993
        :isbn "0201632519"
        :image "Objective_C_Language"))

      (book-list "Languages: Lisp"

       (:title "Practical Common Lisp"
        :link "http://www.gigamonkeys.com/book/"
        :author "Peter Seibel"
        :year 2005
        :isbn "1590592395"
        :image "Practical_Common_Lisp")

       (:title ("Paradigms of Artificial Intelligence Programming:" (br)
                "Case Studies in Common Lisp")
        :link "http://www.norvig.com/paip.html"
        :author "Peter Norvig"
        :year 1992
        :isbn "1558601910"
        :image "Paradigms_of_Artificial_Intelligence_Programming"
        :alt "Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp")

       (:title "Common Lisp: the Language"
        :edition 2
        :link "http://cltl2.lisp.se/"
        :author "Guy L Steele"
        :year 1990
        :isbn "1555580416"
        :image "Common_Lisp_the_Language_2e"
        :alt "Common Lisp: the Language, 2nd ed")

       (:title "COMMON LISP: the Language"
        :edition 1
        :author "Guy L Steele"
        :year 1984
        :isbn "093237641X"
        :image "COMMON_LISP_the_Language"
        :alt "COMMON LISP: the Lanuage, 1st ed")

       (:title "ANSI Common Lisp"
        :link "http://www.paulgraham.com/acl.html"
        :author "Paul Graham"
        :year 1995
        :isbn "0133708756"
        :image "ANSI_Common_Lisp"
        :note (a :href "http://www.cs.northwestern.edu/academics/courses/325/readings/graham/graham-notes.html"
  	       "notes"))

       (:title "On Lisp"
        :link "http://www.paulgraham.com/onlisp.html"
        :author "Paul Graham"
        :year 1993
        :isbn "0130305529"
        :image "On_Lisp")

       (:title "Lisp in Small Pieces"
        :link "http://www-spi.lip6.fr/~queinnec/WWW/LiSP.html"
        :author "Christian Queinnec"
        :year 1996
        :isbn "0521562473"
        :image "Lisp_in_Small_Pieces")

       (:title "The Art of the Metaobject Protocol"
        :link "http://www.lisp.org/mop/"
        :author "Gregor Kiczales"
        :author "Jim des Rivieres"
        :author "Daniel G Bobrow"
        :year 1991
        :isbn "0262610744"
        :image "The_Art_of_the_Metaobject_Protocol")

       (:title "Object-Oriented Programming - The CLOS Perspective"
        :author "Andreas Paepcke"
        :author-note "(ed)"
        :year 1993
        :isbn "0262161362"
        :image "Object-Oriented_Programming")

       (:title "LISP"
        :edition 3
        :link "http://www.ai.mit.edu/people/phw/Books/#Lisp"
        :author "Patrick Henry Winston"
        :author "Berthold Klaus Paul Horn"
        :year 1989
        :isbn "0201083191"
        :image "LISP_3e"
        :alt "LISP, 3rd ed")

       (:title "The Little LISPer"
        :edition 3
        :author "Daniel P Friedman"
        :author "Matthias Felleisen"
        :year 1989
        :isbn "0023397632"
        :image "The_Little_LISPer")

       (:title "LISP 1.5 Programmer's Manual"
	;;:link "http://mhss.nease.net/lisp1.5/mccarthy.html"
	:link "http://community.computerhistory.org/scc/projects/LISP/book/LISP%201.5%20Programmers%20Manual.pdf"
        :edition 2
        :author "John McCarthy"
        :author-note "et al"
        :year 1965
        :isbn "0262130114"
        :image "LISP_1.5_Programmer's_Manual")

       (:title ("Object-Oriented Programming in COMMON LISP:" (br)
                "A Programmer's guide to CLOS")
        :author "Sonja E Keene"
        :year 1989
        :isbn "0201175894"
        :image "Object-Oriented_Programming_in_COMMON_LISP"
        :alt "Object-Oriented Programming in COMMON LISP")

       (:title "Programmering i Lisp"
        :link "http://www.ida.liu.se/~andha/rattelser.html"
        :author "Anders Haraldsson"
        :year 1993
        :isbn "9144396317"
        :image "Programmering_i_Lisp")

       (:title "Performance and Evaluation of Lisp Systems"
        :link "http://www.dreamsongs.com/Books.html"
        :author "Richard P Gabriel"
        :year 1985
        :isbn "0262070936"
        :image "Performance_and_Evaluation_of_Lisp_Systems")

       (:title "Object-Oriented Common LISP"
        :link "http://www.stern.nyu.edu/~sslade/lisp/"
        :author "Stephen Slade"
        :year 1997
        :isbn "0136059406"
        :image "Object-Oriented_Common_LISP")

       (:title "LISP Lore: A Guide to Programming the LISP Machine"
        :author "Hank Bromely"
        :author "Richard Lamson"
        :year 1987
        :isbn "0898382289"
        :image "LISP_Lore")

       (:title "Common Lisp: The Reference"
        :author "Franz, Inc"
        :year 1988
        :isbn "0201114585"
        :image "Common_Lisp_Reference"))

      (book-list "Languages: Miscellaneous"

       (:title "The SR Programming Language"
        :link "http://www.cs.arizona.edu/sr/"
        :author "Gregory R Andrews"
        :author "Ronald A Olsson"
        :year 1993
        :isbn "0805300880"
        :image "The_SR_Programming_Language")

       (:title "ML for the working programmer"
        :edition 1
        :link "http://www.cl.cam.ac.uk/users/lcp/MLbook/"
        :author "Lawrence C Paulson"
        :year 1991
        :isbn "0521422256"
        :image "ML_for_the_Working_Programmer_2e"
        :alt "ML for the Working Programmer, 2nd ed")

       (:title "Ada fr&aring;n b&ouml;rjan"
        :edition 2
        :author "Jan Skansholm"
        :year 1993
        :isbn "9144252323"
        :image "Ada_fran_borjan")

       (:title "Tcl and the Tk Toolkit"
        :author "John K Ousterhout"
        :year 1994
        :isbn "020163337X"
        :image "Tcl_and_the_Tk_Toolkit")

       (:title "Starting FORTH"
        :edition 1
        :link "http://home.earthlink.net/~lbrodie/books.html"
        :author "Leo Brodie"
        :year 1981
        :isbn "0138429227"
        :image "Starting_FORTH"
        :note (a :href "http://home.iae.nl/users/mhx/sf.html" "web clone"))

       (:title "Thinking FORTH"
        :edition 2
        ;:link "http://home.earthlink.net/~lbrodie/books.html"
        :link "http://thinking-forth.sf.net/"
        :author "Leo Brodie"
        :year 1994
        :isbn "0935533001"
        :image "Thinking_FORTH")

       (:title "The Java Programming Language"
        :edition 2
        :link "http://java.sun.com/docs/books/javaprog/"
        :author "Ken Arnold"
        :author "James Gosling"
        :year 1998
        :isbn "0201310066"
        :image "The_Java_Programming_Language_2e")

       (:title "The Java Virtual Machine Specification"
        :edition 2
        :link "http://java.sun.com/docs/books/vmspec/"
        :author "Tim Lindholm"
        :author "Frank Yellin"
        :year 1999
        :isbn "0201432943"
        :image "The_Java_Virtual_Machine_Specification_2e")

       (:title "Dylan Programming"
        :link "http://www.gwydiondylan.org/books/dpg/"
        :author "Neal Feinberg"
        :author "Sonya Keene"
        :author "Robert Mathews"
        :author "Tucker Withington"
        :year 1997
        :isbn "0201479761"
        :image "Dylan_Programming")

       (:title "Smalltalk-80: The Language and Its Implementation"
        :edition 1
        :author "Adele Goldberg"
        :author "David Robson"
        :year 1983
        :isbn "0201113716"
        :image "Smalltalk-80")

       (:title "Javascript: The Definitive Guide"
        :edition 4
        :link "http://dionysia.org/~cfd/resources/webprog/jscript/"
        :author "David Flanagan"
        :year 2001
        :isbn "0596000480"
        :image "Javascript_The_Definitive_Guide"))

      (book-list "Graphics"

       (:title "Interactive Computer Graphics"
        :author "Peter Burger"
        :author "Duncan Gillies"
        :year 1989
        :isbn "0201174391"
        :image "Interactive_Computer_Graphics")

       (:title "The Science of Fractal Images"
        :author "Heinz-Otto Peitgen"
        :author "Dietmar Saupe"
        :author-note "(eds)"
        :year 1988
        :isbn "0387966080"
        :image "The_Science_of_Fractal_Images")

       (:title "Principles of Digital Image Synthesis"
        :link "http://www.glassner.com/andrew/writing/books/podis.htm"
        :author "Andrew Glassner"
        :year 1995
        :isbn "1558602763"
        :image "Principles_of_Digital_Image_Synthesis")

       (:title "Advanced Global Illumination"
        :link "http://www.advancedglobalillumination.com/"
        :author "Philip Dutré"
        :author "Philippe Bekaert"
        :author "Kavita Bala"
        :year 2003
        :isbn "1568811772"
        :image "Advanced_Global_Illumination")

       (:title "Physically Based Rendering"
        :link "http://www.pbrt.org/"
        :author "Matt Pharr"
        :author "Greg Humphreys"
        :year 2004
        :isbn "012553180X"
        :image "Physically_Based_Rendering"))

      (book-list "Systems"

       (:title "Advanced Programming in the UNIX Environment"
        :link "http://www.kohala.com/start/apue.html"
        :author "W Richard Stevens"
        :year 1992
        :isbn "0201563177"
        :image "Advanced_Programming_in_the_UNIX_Environment")

       (:title "The Design and Implementation of the 4.3BSD UNIX
                 Operating System"
        :author "Leffler"
        :author "McKusick"
        :author "Karels"
        :author "Quarterman"
        :year 1989
        :isbn "0201061961"
        :image "The_Design_and_Implementation_of_the_4.3BSD_UNIX_Operating_System")

       (:title "Operating System Concepts"
        :edition 4
        :link "http://cs-www.cs.yale.edu/homes/avi/os-book/osc/"
        :author "Abraham Silberschatz"
        :author "Peter Galvin"
        :year 1994
        :isbn "0201592924"
        :image "Operating_System_Concepts_4e")
  	    
       (:title "The X Window System - Programming and Applications with Xt"
        :edition 2
        :author "Douglas A Young"
        :year 1994
        :isbn "0131238035"
        :image "The_X_Window_System")
  	    
       (:title "Compilers: Principles, Techniques, and Tools"
        :author "Alfred V Aho"
        :author "Ravi Sethi"
        :author "Jeffrey D Ullman"
        :year 1985
        :isbn "0201101947"
        :image "Compilers")
  	    
       (:title "Garbage Collection"
        :link "http://www.cs.kent.ac.uk/people/staff/rej/gcbook/gcbook.html"
        :author "Richard Jones"
        :author "Rafael Lins"
        :year 1996
        :isbn "0471941484"
        :image "Garbage_Collection")
  	    
       (:title "Applied Cryptography"
        :link "http://www.counterpane.com/applied.html"
        :author "Bruce Schneier"
        :year 1996
        :isbn "0471117099"
        :image "Applied_Cryptography")
  	    
       (:title "Basic Methods of Cryptography"
        :author "Jan C A van der Lubbe"
        :year 1998
        :isbn "0521555590"
        :image "Basic_Methods_of_Cryptography")
  	    
       (:title "Amiga Intern"
        :author "Christian Kuhnert"
        :author "Stefan Maelger"
        :author "Johannes Schemmel"
        :year 1992
        :isbn "1557551480"
        :image "Amiga_Intern")
  	    
       (:title "Amiga 3D Graphic Programming in BASIC"
        :author "Bruno Jennrich"
        :author "Andreas Massmann"
        :author "Peter Schultz"
        :year 1989
        :isbn "1557550441"
        :image "Amiga_3D_Graphic_Programming_in_BASIC")
  	    
       (:title "The Atari Compendium"
        :edition 2
        :author "Scott Sanders"
        :year 1994
        :isbn "0963833103"
        :image "The_Atari_Compendium_1e"
        :alt "The Atari Compendium, 1st ed")
  	    
       (:title "Atari ST Internals: The Authoritative Insider's Guide"
        :edition 3
        :author "Rolf Br&uuml;ckmann"
        :author "L Englisch"
        :author "K Gerits"
        :year 1988
        :isbn "0916439461"
        :image "Atari_ST_Internals")
  	    
       (:title "Artificial Intelligence: A Modern Approach"
        :link "http://aima.cs.berkeley.edu/"
        :author "Stuart Russell"
        :author "Peter Norvig"
        :year 1995
        :isbn "0131038052"
        :image "Artificial_Intelligence"))

      (book-list "Processors"

       (:title "ARM Architecture Reference Manual"
        :edition 2
        :author "David Jagger"
        :author "David Seal"
        :year 2000
        :isbn "0201737191"
        :image "ARM_Architecture_Reference_Manual")

       (:title "Programmer's Reference Manual"
        :author "Motorola, Inc"
        :year 1992
        :image "Programmer's_Reference_Manual"
        :note "M68000 series")

       (:title "M68040 User's Manual"
        :author "Motorola, Inc"
        :year 1993
        :image "M68040_User's_Manual"
        :note (progn
  	      (write-string "also ")
  	      (a :href "books/MC68040_User's_Manual.jpg" "1989 edition")))

       (:title "MC68030 User's Manual"
        :edition 2
        :author "Motorola, Inc"
        :year 1990
        :isbn "0135664233"
        :image "MC68030_User's_Manual")

       (:title "DSP56000/DSP56001 User's Manual"
        :author "Motorola, Inc"
        :year 1990
        :image "DSP56k_User's_Manual")
        
       (:title ("PowerPC Microprocessor Family:" (br)
                "The Programming Environments for 32-Bit Microprocessors")
        :author "Motorola, Inc"
        :year 1997
        :image "PowerPC_Programming_Environments"
        :alt "PowerPC Microprocessor Family: The Programming Environments for 32-Bit Microprocessors")

       (:title "MPC7410 RISC Microprocessor User's Manual"
        :author "Motorola, Inc"
        :year 2000)

       (:title "MPC7400 PowerPC Microprocessor User's Manual"
        :author "Motorola, Inc"
        :year 1999)

       (:title "MPC750 RISC Microprocessor User's Manual"
        :author "Motorola, Inc"
        :year 1997)

       (:title "Altivec Programming Interface Manual"
        :author "Motorola, Inc"
        :year 1999)

       (:title "Altivec Programming Environments Manual"
        :author "Motorola, Inc"
        :year 1998 )

       (:title "AMD x86-64 Architecture Programmer's Manual"
        :link "http://www.amd.com/us-en/Processors/DevelopWithAMD/0,,30_2252_875_7044,00.html"
        :author "AMD"
        :year 2002
        :image "x86-64"))

      (book-list "Culture"

       (:title "Hackers - Heroes of the Computer Revolution"
        :author "Steven Levy"
        :year 1984
        :isbn "0140232699"
        :image "Hackers")

       (:title "The UNIX-HATERS Handbook"
        :link "http://research.microsoft.com/~daniel/unix-haters.html"
        :author "Garfinkel"
        :author "Weise"
        :author "Strassman"
        :year 1994
        :isbn "1568842031"
        :image "The_UNIX-HATERS_Handbook")

       (:title "A Quarter Century of UNIX"
        :author "Peter H Salus"
        :year 1994
        :isbn "0201547775"
        :image "A_Quarter_Century_of_UNIX")

       (:title "The New Hacker's Dictionary"
        :edition 1
        :author "Eric S Raymond"
        :author "Guy L Steele"
        :author-note "(eds)"
        :year 1991
        :isbn "0262680696"
        :image "The_New_Hacker's_Dictionary")

       (:title "The Mythical Man-Month"
        :edition "anniversary"
        :author "Frederick P Brooks"
        :year 1995
        :isbn "0201835959"
        :image "The_Mythical_Man-Month")

       (:title "Patterns of Software"
        :link "http://www.dreamsongs.com/Books.html"
        :author "Richard P Gabriel"
        :year 1996
        :isbn "019510269X"
        :image "Patterns_of_Software"
        :alt "Patterns of Software")

       (:title "In the Beginning was the Command Line"
        :author "Neal Stephenson"
        :year 1999
        :isbn "0380815931"
        :image "In_the_Beginning_was_the_Command_Line")

       (:title "De stora programerarna"
        :author "Susan Lammers"
        :year 1986
        :isbn "9179420273"
        :image "De_stora_programmerarna"
        :note (progn
  	      (write-string "Programmers at Work, ")
  	      (isbn "1556150148"))))

      (book-list "Miscellaneous"

       (:title "G&ouml;del, Escher, Bach: an Eternal Golden Braid"
        :author "Douglas Hofstadter"
        :year 1979
        :isbn "0465026567"
        :image "Godel,_Escher,_Bach")

       (:title "Metamagical Themas: Questing for the Essence of Mind and Pattern"
        :author "Douglas Hofstadter"
        :year 1985
        :isbn "0465045669"
        :image "Metamagical_Themas")

       (:title "The Unfolding of Language"
	:link "http://www.unfoldingoflanguage.com/"
        :author "Guy Deutscher"
        :year 2005
        :isbn "0805079076"
	:image "The_Unfolding_of_Language"
        #| :isbn-13 "9780805079074" |#))

      (tr (td :colspan 3 (h2 (center "Shopping list")))))

    (ul
     "First some Lisp books by publication date:"
     (li
      "[2005?] "
      (a :href "http://www.paulgraham.com/" "Paul Graham") ": "
      (a :href "http://www.paulgraham.com/onlisp.html" "On Lisp") " "
      "[" (a :href "http://groups.google.com/groups?selm=add24301.0306271142.2b67e898%40posting.google.com"
 	    "reprint") ", "
 	 (a :href "http://www.apress.com/" "publisher") "]")
     (li
      "[2005?] "
      (a :href "http://www.psg.com/~dlamkins/" "Lamkins") ": "
      (a :href "http://www.psg.com/~dlamkins/sl/cover.html" "Successful Lisp")
      "; " (isbn "3937526005")
      " [" (a :href "http://www.bookfix.com/1163.html" "publisher") "]")
     (li
      "[2005?] "
      (a :href "http://ww.telent.net/" "Barlow") ": "
      (a :href "http://google.com/groups?selm=87n0gzdlwu.fsf@noetbook.telent.net"
 	"Lisp for Perl Programmers (?)")
      " (" (a :href "http://google.com/groups?selm=6PeDa.896$ut4.42197272@newssvr15.news.prodigy.com" "or here") ")")
     (li
      "[2005?] "
      (a :href "http://franz.com/" "Franz") ": "
      (a :href "https://secure.franz.com/store/home" "Basic Lisp Techniques")
      " [" (a :href "http://www.franz.com/resources/educational_resources/cooper.book.pdf" "PFD") "]")
     (li
      "[2005?]
       David Margolies: The ANSI Common Lisp Reference Book; "
      (isbn "1590592204")
      " [" (a :href "http://groups.google.com/groups?selm=add24301.0306271142.2b67e898%40posting.google.com" "info") ", "
      (a :href "http://www.apress.com/book/bookDisplay.html?bID=263"
 	"publisher") "]")
     (li
      "[2002]
       Yuasa, Okuno, Okuna &amp; Yuasa (eds): Advanced Lisp Technology")
     (li
      "[2002] "
      (a :href "http://www.frii.com/~wmilner/" "Milner") ": "
      "Common Lisp Dictionary ["
      (a :href "http://groups.google.com/groups?selm=857khue499.fsf@junk.nocrew.org" "is this for real?") "]")
     (li
      "[2001] "
      (a :href "http://www.cs.indiana.edu/~dfried/" "Friedman") ", "
      (a :href "http://www.ccs.neu.edu/home/wand/" "Wand") " &amp; "
      (a :href "http://www.cs.indiana.edu/~chaynes/" "Haynes") ": "
      (a :href "http://www.cs.indiana.edu/eopl/"
 	"Essentials of Programming Languages")
      "; "
      (isbn "0262062178")
      " [hairy Scheme book]")
     (li
      "[2000] "
      (a :href "http://www.lambdacs.com/bil/bil.html" "Bil Lewis") ", "
      "Dan LaLiberte &amp; "
      (a :href "http://stallman.org/" "Richard M Stallman") ": "
      (a :href "http://www.gnu.org/doc/book3.html"
 	"GNU Emacs Lisp Reference Manual") "; "
      (isbn "1882114736"))
     (li
      "[1997] " (s "Slade: Object-Oriented Common Lisp"))
     (li
      "[1996] " (s "Haraldsson: Programmering i Lisp"))
     (li
      "[1996] "
      (s "Harold Abelson &amp; Gerald Sussman: Structure and
          Interpretation of Computer Programs"))
     (li
      "[1996] " (s "Queinnec: Lisp in Small Pieces"))
     (li
      "[1996] " (s "Gabriel: Patterns of Software"))
     (li
      "[1996] R Kent Dybvig: "
      (a :href "http://www.scheme.com/tspl2d/"
 	"The Scheme Programming Language, 2nd ed") "; "
      (isbn "0134546466"))
     (li
      "[1995] "
      (a :href "http://www.cs.indiana.edu/~dfried/" "Friedman") " &amp; "
      (a :href "http://www.ccs.neu.edu/home/matthias/" "Felleisen") ": "
      (a :href "http://www.ccs.neu.edu/home/matthias/BTLS/"
 	"The Little Schemer, 4th ed"))
     (li
      "[1995] "
      (a :href "http://www.cs.indiana.edu/~dfried/" "Friedman") " &amp; "
      (a :href "http://www.ccs.neu.edu/home/matthias/" "Felleisen") ": "
      (a :href "http://www.ccs.neu.edu/home/matthias/BTSS/"
 	"The Seasoned Schemer"))
     (li
      "[1995] " (s "Paul Graham: ANSI Common Lisp"))
     (li
      "[1994] McKay, York, et al: "
      (a :href "http://www.mikemac.com/mike/clim/cover.html"
 	"Common Lisp Interface Manager Specification"))
     (li
      "[1993] " (s "Paul Graham: On Lisp"))
     (li
      "[1992] "
      (s "Norvig: Paradigms of Artificial Intelligence Programming
          - Case Studies in Common Lisp"))
     (li
      "[1992] "
      (a :href "http://www.cse.buffalo.edu/pub/WWW/faculty/shapiro/" "Shapiro")
      ": "
      (a :href "http://www.cse.buffalo.edu/pub/WWW/faculty/shapiro/Commonlisp/"
 	"Common Lisp - An Interactive Approach") "; "
      (isbn "0716782189"))
     (li
      "[1991] "
      "Watson: Common Lisp Modules - "
      "Artificial Intelligence in the Era of Neural Networks and Chaos Theory; "
      (isbn "0387976140"))
     (li
      "[1991] "
      (s "Kicsales, Rivieres &amp; Bobrow: The Art of the Metaobject Protocol"))
     (li
      "[1991] Peter M Kooge: The Architecture of Symbolic Computing; "
      (isbn "0070355967"))
     (li
      "[1990] " (s "Steele: Common Lisp the Language, 2nd ed"))
     (li
      "[1990] Koschmann: The Common LISP Companion; "
      (isbn "0471503088"))
     (li
      "[1990] Stark: Lisp, Lore and Logic; "
      (isbn "038797072X"))
     (li
      "[1990] Benson &amp; Miller: Lisp Style and Design; "
      (isbn "1555580440")
      " ["
      (a :href "http://www.amazon.co.uk/exec/obidos/account-access-login/"
 	"on order") "]")
     (li
      "[1989] "
      (a :href "http://www.cs.cmu.edu/~dst/" "Touretzky") ": "
      (a :href "http://www.cs.cmu.edu/~dst/LispBook/"
 	"Common Lisp - A Gentle Introduction to Symbolic Computation") "; "
      (isbn "0805304924"))
     (li
      "[1989] Springer &amp; Friedman: Scheme and the Art of Programming
       [hairy Scheme book]")
     (li
      "[1989] "
      (s "Keene: Object-Oriented Programming in Common Lisp -
          A Programmer's guide to CLOS"))
     (li
      "[1989] " (s "Friedman, Felleisen: The Little Lisper, 3rd ed"))
     (li
      "[1988] " (s "Winston &amp; Horn: Lisp, 3rd ed"))
     (li
      "[1988] " (s "Franz, Inc: Common Lisp - The Reference"))
     (li
      "[1987] "
      (s "Bromely &amp; Lamson: LISP Lore -
          A Guide to Programming the LISP Machine"))
     (li
      "[1986] Robert Wilensky: Common LISPcraft; "
      (isbn "0393955443"))
     (li
      "[1985] "
      (s "Richard Gabriel: Performance and Evaluation of Lisp Systems"))
     (li
      "[1984] " (s "Steele: Common Lisp the Language (1st ed)"))
     (li
      "[1978] Allen: Anatomy of Lisp; " (isbn "007001115X"))
     (li
      "[?] Marti, Hearn, Griss &amp; Griss: "
      (a :href "http://www.uni-koeln.de/REDUCE/3.6/doc/sl/"
 	"The Standard Lisp Report"))

     (li "The Brain Makers [AI history, many errors?]")
     (li
      (a :href "http://www.finseth.com/" "Craig A Finseth") ": "
      (a :href "http://www.finseth.com/~fin/craft/"
 	"The Craft of Text Editing") "; "
      (isbn "3540976167") ", " (isbn "0387976167"))
     (li
      "Appel: "
      (a :href "http://www.cs.princeton.edu/~appel/papers/cwc.html"
 	"Compiling with Continuations")
      "; "
      (isbn "0521416957"))
     (li "Goldberg &amp; Robson: Smalltalk 80 - The Language; "
 	(isbn "0201136880"))
     (li "Skublics, Klimas &amp; Thomas: Smalltalk with Style; "
 	(isbn "0131655493"))
     (li "Chamond Liu: "
 	(a :href "http://www.browsebooks.com/Liu/"
 	   "Smalltalk, Objects, and Design; ")
 	(isbn "1583484906") " [also " (isbn "1884777279") " and "
 	(isbn "0132683350") "]")
     (li "Kent Beck: Smalltalk Best Practice Patterns" (isbn "013476904X"))
     (li "Thompson: Haskell - The Craft of Functional Programming")
     (li "Cousineau: Functional Approach to Programming")
     (li
      "Edward K Conklin &amp; Elizabeth D. Rather: "
      (a :href "http://forth.com/Content/Handbook/Handbook.html"
 	"Forth Programmer's Handbook") "; "
      (isbn "0966215605"))
     (li
      "Elizabeth D Rather: "
      (a :href "http://forth.com/Content/fat/fat.html"
 	"Forth Application Techniques") "; "
      (isbn "0966215613"))
     (li "Leo Brodie: Starting FORTH, 2nd ed; "
         (isbn "013843087X") ", " (isbn "0138430799"))
     (li
      (a :href "http://www.ece.cmu.edu/~koopman/" "Philip J Koopman") ": "
      (a :href "http://www.ece.cmu.edu/~koopman/stack_computers/index.html"
 	"Stack Computers - The New Wave") "; "
      (isbn "0138379238") ", " (isbn "0470214678"))
     (li "Adobe: PostScript Language Reference Manual")
     (li "Mayer: Eiffel - The Language")
     (li "Clocksin &amp; Mellish: Programming in Prolog [the classic]")
     (li "O'Keefe: The Craft of Prolog [advanced techniques]")
     (li "Maier &amp; Warren: Computing with Logic [Prolog implementation]")
     (li
      "Andrew Shalit, David Moon &amp; "
      (a :href "http://www6.uniovi.es/java-http/people/orca/"
 	"Orca Starbuck") ": "
      (a :href "http://www.gwydiondylan.org/books/drm/"
 	"Dylan Reference Manual") "; "
      (isbn "0201442116"))
     (li "Iain D Craig: Programming in Dylan; " (isbn "3540760539"))
     ;;(li "?: JavaScript: The Definitive Guide; " (isbn "0596000480"))
     "&nbsp;"
     (li "Sweetman: See MIPS Run; " (isbn "1558604103"))
     (li "SPARC: SPARC Architecture Manual - Version 9; " (isbn "0130992275"))
     "&nbsp;"
     (li "Lions: Lion's Commentary on UNIX with Source Code; "
 	(isbn "1573980137"))
     (li "Vahalia: UNIX Internals - The New Frontiers ")
     (li "McKusick, Bostic, Karels &amp; co: The Design and Implementation
          of the 4.4BSD Operating System")
     (li "Bach: Design of the Unix Operating System")
     (li "Mauro &amp; McDougall: Solaris Internals - Core Kernel Architecture")
     (li
      "Schimmel: UNIX Systems for Modern Architectures: Symmetric
       Multiprocessing and Caching for Kernel Programmers")
     (li
      (a :href "http://www.mcs.anl.gov/~gropp" "William Gropp") ", "
      (a :href "http://www.mcs.anl.gov/home/lusk" "Ewing Lusk") " &amp; "
      (a :href "http://www.cs.msstate.edu/~tony/" "Anthony Skjellum") ": "
      (a :href "http://www-unix.mcs.anl.gov/mpi/usingmpi/" "Using MPI") "; "
      (isbn "0262571323"))
     (li
      (a :href "http://www.mcs.anl.gov/~gropp" "William Gropp") ", "
      (a :href "http://www.mcs.anl.gov/home/lusk" "Ewing Lusk") " &amp; "
      (a :href "http://www.mcs.anl.gov/~thakur" "Rajeev Thakur") ": "
      (a :href "http://www-unix.mcs.anl.gov/mpi/usingmpi2/" "Using MPI-2") "; "
      (isbn "0262571331"))
     (li "Muchnick: Advanced Compiler Design and Implementation")
     (li "Grune, Bal, Jacobs &amp; Kalngendoen: Modern Comiler Design")
     (li "Pittman: Art of Compiler Design - Theory and Practice")
     (li
      "Appel: "
      (a :href "http://www.cs.princeton.edu/~appel/modern/"
 	"Modern Compiler Implementation in ..."))
     (li "Randy Allen: Optimizing Compilers for Modern Architectures")
     (li
      "Donald Knuth: Selected Papers on Computer Science; "
      (isbn "1881526925"))
     (li
      "Edsger W Dijkstra: A Discipline of Programming; "
      (isbn "013215871X"))
     "&nbsp;"
     (li
      (a :href "http://www.plauger.com/" "Plauger") ": "
      "Programming on Purpose II - Essays on Software People; "
      (isbn "0133281051") (br)
      (a :href "http://www.plauger.com/" "Plauger") ": "
      "Programming on Purpose III - Essays on Software Technology; "
      (isbn "0133281132"))
     (li
      "Hunt &amp; Thomas: The Pragmatic Programmer; "
      (isbn "020161622X"))
     (li
      (b "Bentley: More Programming Pearls - Confessions of a Coder") "; "
      (isbn "0201118890"))
     (li
      (b "Felleisen, Findler, Flatt &amp; Krishnamurthi: "
 	(a :href "http://www.htdp.org/" "How to Design Programs")))
     (li
      (a :href "http://www.martinfowler.com/" "Martin Fowler") ": "
      (a :href "http://www.refactoring.com/"
 	"Refactoring - Improving the Design of Existing Code"))
     (li
      "Duncan: "
      (a :href "http://www.showprogramming.com/TheCareerProgrammer.asp"
 	"The Career Programmer - Guerilla Tactics for an Imperfect World"))
     (li
      (a :href "http://www.spinellis.gr/" "Diomidis Spinellis") ", 2003: "
      (a :href "http://www.spinellis.gr/codereading/"
 	"Code Reading: The Open Source Perspective") "; "
      (isbn "0201799405"))
     "&nbsp;"
     (li
      (a :href "http://graphics.stanford.edu/~mmp/" "Matt Pharr") " &amp; "
      (a :href "http://www.cs.virginia.edu/~humper/" "Greg Humphreys")
      ", 2004: "
      (a :href "http://pbrt.org/"
 	"Physically Based Rendering: from Theory to Implementation") "; "
      (isbn "012553180X"))
     (li
      (a :href "http://graphics.ucsd.edu/~henrik/" "Henrik Wann Jensen")
      ", 2001: "
      (a :href "http://graphics.ucsd.edu/~henrik/papers/book/"
 	"Realistic Image Synthesis Using Photon Mapping") "; "
      (isbn "1568811470"))
     (li
      (a :href "http://www.cs.utah.edu/~shirley/" "Peter Shirley") " &amp; "
      (a :href "http://www.cs.princeton.edu/~rmorley/" "Keith Morley")
      ", 2003: "
      "Realistic Ray Tracing, 2nd ed; "
      (isbn "1568811985"))
     (li
      (a :href "http://www.glassner.com/" "Andrew Glassner") ", 1989: "
      (a :href "http://www.glassner.com/andrew/writing/books/irt.htm"
 	"Introduction to Ray Tracing") "; "
      (isbn "0122861604"))
     (li
      "Schneider &amp; Eberly, 2002: Geometric Tools for Computer Graphics; "
      (isbn "1558605940"))
     (li "Graphic Gems; " (isbn "0122861663"))
     (li "Graphic Gems II; " (isbn "0120644819"))
     (li "Graphic Gems III; " (isbn "0124096735"))
     (li "Graphic Gems IV; " (isbn "0123361559"))
     (li "Graphic Gems V; " (isbn "0125434553"))
     "&nbsp;"
     (li "John McWhorter: The Power of Babel: A Natural History of Language"
	 (isbn "006052085X"))
     (li "Ellen Ullman: The Bug")
     (li "The Equation That Couldn't Be Solved: How Mathematical Genius Discovered the Language of Symmetry; "
	 (isbn "0743258207"))
     (li
      "Marti Olsen Laney:
       The Introvert Advantage: How to Thrive in an Extrovert World; "
      (isbn "0761125892"))
     (li"Pirsig: Zen and the Art of Motorcycle Maintenance")
     (li "Oxford English Dictionary"))
    (p
     (a :href "http://eberhard-lutz.bei.t-online.de/classics.html"
        "Classical computer science texts") ".")))
