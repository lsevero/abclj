# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).


## [0.1.4] - 2020-11-08
### Added
- Added Common Lisp HashTable to the cl->clj protocol
- Added IPersistentMap to the clj->cl protocol
- added the 0 arity form of funcall
- added a map->alist function

## [0.1.3] - 2020-11-06
### Added
- Upgraded ABCL version to 1.8.0
- Added abcl-contrib to the dependencies
- added print-method and print-dup for ComplexString
- cl-symbol now supports strings (useful for symbols like cl:1+ that broke the Clj reader)
- added dotted-pair? and dotted-list? predicates
- added list-all-packages function

## [0.1.2] - 2020-11-06
### Added
- print-method and print-dup for Cons
- added Sequential do the Evaluatable protocol
- cl-cons now builds with any sequential
- Improved the defun macro, `defun` defined functions are now allowed to be called inside new `defun` definitions.

## [0.1.1] - 2020-11-06
### Added
- a examples folder
- Keyword and nil are now supported by the CommonLispfiable protocol
- Fixing the Double type on the CommonLispfiable protocol
- the defun macro

## 0.1.0 - 2020-11-04
- First release

