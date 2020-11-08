# game-of-life

Example of how to use Common Lisp, Clojure and Java together.

## Usage

``` shell
lein run
```

## Comments

The file `life.clj` is basically a copy and paste from [Armed Bear
Sample
Code](https://common-lisp.net/project/armedbear/doc/abcl-user.html) on
`life.lisp` file. Which is amazing! The necessary modifications were:


1. Change `1+`, `1-` cl operators by `(- thing 1)` or `(+ thing 1)`
because the LispReader implemented in Clojure does not allow you to
type these symbols.

2. Change `#'` syntactic sugar by `function`. The LispReader again
reserved the usage of `#` to Clojure.

3. Execute `defvar` forms inside a `with-cl`.


## License

Copyright © 2020 Wanderson Ferreira

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
