#+named-readtables
(named-readtables:in-readtable :standard)

(asdf:defsystem :com.andrewsoutar.brace-lambda
  :description "Use braces for expressing lambda functions!"
  :version "1.0.0"
  :author ("Andrew Soutar <andrew@andrewsoutar.com>")
  :maintainer "Andrew Soutar <andrew@andrewsoutar.com>"
  :defsystem-depends-on (:asdf-package-system)
  :class :package-inferred-system
  :depends-on (:com.andrewsoutar.brace-lambda/brace-lambda))
