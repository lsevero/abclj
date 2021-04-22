# Armed Bear Clojure
Dead easy Common Lisp interop

[![Clojars Project](https://img.shields.io/clojars/v/abclj.svg)](https://clojars.org/abclj)

## Why??

There are others of attempts to shorten the gap between clojure and common lisp like [Cloture](https://github.com/ruricolist/cloture) and [clclojure](https://github.com/joinr/clclojure).
Once they are complete Clojure will benefit from native binaries and excelent compilers like SBCL, however they are far from complete.

This project took a different aproach, instead of rewriting the whole Clojure langugage on CL I'm embedding ABCL in Clojure.
Since both are implemented in Java and Clojure has an awesome java interop is easy to have full access on the ABCL Common Lisp environment.
This way we have complete support for both Clojure and Common Lisp.

ABCL is a incredible Common Lisp implementation, the source code is very clear and straightforward.
It provides both a compiler and a interpreter that can be used as scripting language in any java-based project, but embedding it in other lisps (like clojure) I do believe has some advantages.


## Usage

```clojure
(require '[abclj.core :refer :all])


;using the with-cl->clj macro to inject CL code into the interpreter
;it is a composition of the with-cl macro to inject code and
;the cl->clj protocol to convert the CL java class to a clj relative
(= 120 (with-cl->clj
                 '(defun fact (n)
                    (reduce (function *) (loop for i from 1 to n collect i)))
                 '(fact 5)))

;you can also evaluate strings and LispObjects
(= 3 (cl->clj (cl-evaluate "(+ 1 2)")))
;the cl-cons creates a CL cons from a clj sequential
(= 6 (cl->clj (cl-evaluate (cl-cons [(cl-symbol 'cl/+) 1 2 3 cl-nil]))))

;importing CL functions to the java/clj world
;the symbol namespaces are used to search for CL packages
(def cl-format (getfunction 'cl/format))
(-> cl-format
  (funcall cl-nil (cl-string "Hi from CL, ~a") (cl-string "armed bear clojure"))
  cl->clj
  clojure.string/upper-case
  println) ;=> HI FROM CL, ARMED BEAR CLOJURE


;quicklisp support
(require '[abclj.quicklisp :refer [quickload]])

(quickload :trivial-http :drakma)
(with-cl '(trivial-http:http-get "http://lite.duckduckgo.com/lite/"))
(with-cl '(drakma:http-request "http://lisp.org/"))

(defun fac2 (n)
  (reduce (function *) (loop for i from 1 to n collect i)))

(fac2 (fac2 3))

(defun discriminant (a b c)
  (- (* b b) (* 4 a c)))

(discriminant 1 2 3)
```
Also check the project tests and examples.

## The ultimate goal

The reason I wanted to see Clojure and Common Lisp working with each other was to use CL programs/libraries on Clojure, especially Maxima and ACL2.
~~Since ABCL already compiles and runs Maxima it should be possible but we are very far from it 🤷.~~
I've got ABCLJ working with maxima on the project [clj-maxima](https://github.com/lsevero/clj-maxima)

## License

Copyright © 2020 Lucas Severo

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
