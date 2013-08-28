(in-package #:locker-tests)

(defparameter *dummy-list*
  (list "HTTP://HACKERSHELF.COM/BROWSE/" "ETHERCALC HTTPS://ETHERCALC.ORG/UWR6LQ1SAT"
 "APPLIED CRYPTOGRAPHY, BRUCE SCHNEIER" "SECRETS OF THE JAVASCRIPT NINJA"
 "SMALLTALK BEST PRACTICE PATTERNS"
 "REFACTORING: IMPROVING THE DESIGN OF EXISTING CODE"
 "MANAGING PROJECTS WITH GNU MAKE, 3RD ED, ROBERT MECKLENBURG (OREILLY OPENBOOK)"
 "LINUX DEVICE DRIVERS, 3RD ED, JONATHAN CORBET (OREILLY OPENBOOK)"
 "APPRENTICESHIP PATTERNS, DAVE HOOVER (OREILLY OPENBOOK)"
 "THE PRACTICE OF PROGRAMMING, KERNIGHAN AND PIKE"
 "SF THE CHRONICLES OF AMBER, ROGER ZELAZNY" "SF QUARANTINE, GREG EGAN"
 "SF A FIRE UPON THE DEEP, VERNOR VINGE" "SF FARENHEIT 451, RAY BRADBURY"
 "SF ORYX AND CRAKE, MARGARET ATWOOD" "SF ARMOR, JOHN STEAKLEY"
 "SF ANATHEM. NEAL STEPHENSON"
 "SF THE LEFT HAND OF DARKNESS, URSULA K. LE GUIN"
 "SF THE SIRENS OF TITAN, KURT VONNEGUT" "SF CONTACT, CARL SAGAN"
 "SF THE STARS MY DESTINATION, ALFRED BESTER"
 "SF THE MOTE IN GOD'S EYE, LARRY NIVEN & JERRY POURNELLE"
 "SF CONSIDER PHLEBAS, IAIN M. BANKS"
 "SF DO ANDROIDS DREAM OF ELECTRIC SHEEP, PHILIP K. DICK"
 "SF CHILDHOOD'S END, ARTHUR C. CLARKE" "SF SNOW CRASH, NEAL STEPHENSON"
 "MOONWALKING WITH EINSTEIN, JOSHUA FOER" "BEYOND FEAR, BRUCE SCHNEIER"))

(defparameter *dummy-file*
  "
* BOOKS
BEYOND FEAR, BRUCE SCHNEIER
MOONWALKING WITH EINSTEIN, JOSHUA FOER
SF SNOW CRASH, NEAL STEPHENSON
SF CHILDHOOD'S END, ARTHUR C. CLARKE
SF DO ANDROIDS DREAM OF ELECTRIC SHEEP, PHILIP K. DICK
SF CONSIDER PHLEBAS, IAIN M. BANKS
SF THE MOTE IN GOD'S EYE, LARRY NIVEN & JERRY POURNELLE
SF THE STARS MY DESTINATION, ALFRED BESTER
SF CONTACT, CARL SAGAN
SF THE SIRENS OF TITAN, KURT VONNEGUT
SF THE LEFT HAND OF DARKNESS, URSULA K. LE GUIN
SF ANATHEM. NEAL STEPHENSON
SF ARMOR, JOHN STEAKLEY
SF ORYX AND CRAKE, MARGARET ATWOOD
SF FARENHEIT 451, RAY BRADBURY
SF A FIRE UPON THE DEEP, VERNOR VINGE
SF QUARANTINE, GREG EGAN
SF THE CHRONICLES OF AMBER, ROGER ZELAZNY

* COMSCI BOOKS
THE PRACTICE OF PROGRAMMING, KERNIGHAN AND PIKE
APPRENTICESHIP PATTERNS, DAVE HOOVER (OREILLY OPENBOOK)
LINUX DEVICE DRIVERS, 3RD ED, JONATHAN CORBET (OREILLY OPENBOOK)
MANAGING PROJECTS WITH GNU MAKE, 3RD ED, ROBERT MECKLENBURG (OREILLY OPENBOOK)
REFACTORING: IMPROVING THE DESIGN OF EXISTING CODE
SMALLTALK BEST PRACTICE PATTERNS
SECRETS OF THE JAVASCRIPT NINJA
APPLIED CRYPTOGRAPHY, BRUCE SCHNEIER

* SITES
HTTP://HACKERSHELF.COM/BROWSE/



")

(define-test split-space-test
  (assert-equal (list "hello" "world") (split-space "hello world")))

(define-test split-equal-test
  (assert-equal (list "file" "default") (split-equal "file=default")))

(define-test split-newline-test
  (assert-equal (list "* banks bank1 bank2" "* docs doc1") (split-newline (format nil "* banks bank1 bank2 ~% * docs doc1"))))

(define-test split-header-test
  (assert-equal (list "" "* banks bank1 bank2" "* docs doc1") (split-header "* banks bank1 bank2 * docs doc1")))

(define-test find-item-test-single
  (assert-equal (list "SMALLTALK BEST PRACTICE PATTERNS") (find-item "smalltalk" *dummy-list*)))

(define-test item-list-test
  (assert-equal '("BEYOND FEAR, BRUCE SCHNEIER" "MOONWALKING WITH EINSTEIN, JOSHUA FOER"
 "SF SNOW CRASH, NEAL STEPHENSON" "SF CHILDHOOD'S END, ARTHUR C. CLARKE"
 "SF DO ANDROIDS DREAM OF ELECTRIC SHEEP, PHILIP K. DICK"
 "SF CONSIDER PHLEBAS, IAIN M. BANKS"
 "SF THE MOTE IN GOD'S EYE, LARRY NIVEN & JERRY POURNELLE"
 "SF THE STARS MY DESTINATION, ALFRED BESTER" "SF CONTACT, CARL SAGAN"
 "SF THE SIRENS OF TITAN, KURT VONNEGUT"
 "SF THE LEFT HAND OF DARKNESS, URSULA K. LE GUIN"
 "SF ANATHEM. NEAL STEPHENSON" "SF ARMOR, JOHN STEAKLEY"
 "SF ORYX AND CRAKE, MARGARET ATWOOD" "SF FARENHEIT 451, RAY BRADBURY"
 "SF A FIRE UPON THE DEEP, VERNOR VINGE" "SF QUARANTINE, GREG EGAN"
 "SF THE CHRONICLES OF AMBER, ROGER ZELAZNY"
 "THE PRACTICE OF PROGRAMMING, KERNIGHAN AND PIKE"
 "APPRENTICESHIP PATTERNS, DAVE HOOVER (OREILLY OPENBOOK)"
 "LINUX DEVICE DRIVERS, 3RD ED, JONATHAN CORBET (OREILLY OPENBOOK)"
 "MANAGING PROJECTS WITH GNU MAKE, 3RD ED, ROBERT MECKLENBURG (OREILLY OPENBOOK)"
 "REFACTORING: IMPROVING THE DESIGN OF EXISTING CODE"
 "SMALLTALK BEST PRACTICE PATTERNS" "SECRETS OF THE JAVASCRIPT NINJA"
 "APPLIED CRYPTOGRAPHY, BRUCE SCHNEIER" "HTTP://HACKERSHELF.COM/BROWSE/")
                (item-list (mapcar #'parse-entries (sanitize *dummy-file*)))))

(define-test show-category-test
  (assert-equal '(|* BOOKS|
 (|BEYOND FEAR, BRUCE SCHNEIER| |MOONWALKING WITH EINSTEIN, JOSHUA FOER|
  |SF SNOW CRASH, NEAL STEPHENSON| |SF CHILDHOOD'S END, ARTHUR C. CLARKE|
  |SF DO ANDROIDS DREAM OF ELECTRIC SHEEP, PHILIP K. DICK|
  |SF CONSIDER PHLEBAS, IAIN M. BANKS|
  |SF THE MOTE IN GOD'S EYE, LARRY NIVEN & JERRY POURNELLE|
  |SF THE STARS MY DESTINATION, ALFRED BESTER| |SF CONTACT, CARL SAGAN|
  |SF THE SIRENS OF TITAN, KURT VONNEGUT|
  |SF THE LEFT HAND OF DARKNESS, URSULA K. LE GUIN|
  |SF ANATHEM. NEAL STEPHENSON| |SF ARMOR, JOHN STEAKLEY|
  |SF ORYX AND CRAKE, MARGARET ATWOOD| |SF FARENHEIT 451, RAY BRADBURY|
  |SF A FIRE UPON THE DEEP, VERNOR VINGE| |SF QUARANTINE, GREG EGAN|
  |SF THE CHRONICLES OF AMBER, ROGER ZELAZNY|))
                (show-category "* books" (mapcar #'parse-entries (sanitize *dummy-file*)))))

(define-test find-item-test-multi
  (assert-equal (list "SF THE CHRONICLES OF AMBER, ROGER ZELAZNY" "SF QUARANTINE, GREG EGAN"
                      "SF A FIRE UPON THE DEEP, VERNOR VINGE" "SF FARENHEIT 451, RAY BRADBURY"
                      "SF ORYX AND CRAKE, MARGARET ATWOOD" "SF ARMOR, JOHN STEAKLEY"
                      "SF ANATHEM. NEAL STEPHENSON"
                      "SF THE LEFT HAND OF DARKNESS, URSULA K. LE GUIN"
                      "SF THE SIRENS OF TITAN, KURT VONNEGUT" "SF CONTACT, CARL SAGAN"
                      "SF THE STARS MY DESTINATION, ALFRED BESTER"
                      "SF THE MOTE IN GOD'S EYE, LARRY NIVEN & JERRY POURNELLE"
                      "SF CONSIDER PHLEBAS, IAIN M. BANKS"
                      "SF DO ANDROIDS DREAM OF ELECTRIC SHEEP, PHILIP K. DICK"
                      "SF CHILDHOOD'S END, ARTHUR C. CLARKE" "SF SNOW CRASH, NEAL STEPHENSON") (find-item "sf" *dummy-list*)))

(define-test sanitize-test
  (assert-equal  '(("* BOOKS" "BEYOND FEAR, BRUCE SCHNEIER"
                                 "MOONWALKING WITH EINSTEIN, JOSHUA FOER" "SF SNOW CRASH, NEAL STEPHENSON"
                                 "SF CHILDHOOD'S END, ARTHUR C. CLARKE"
                                 "SF DO ANDROIDS DREAM OF ELECTRIC SHEEP, PHILIP K. DICK"
                                 "SF CONSIDER PHLEBAS, IAIN M. BANKS"
                                 "SF THE MOTE IN GOD'S EYE, LARRY NIVEN & JERRY POURNELLE"
                                 "SF THE STARS MY DESTINATION, ALFRED BESTER" "SF CONTACT, CARL SAGAN"
                                 "SF THE SIRENS OF TITAN, KURT VONNEGUT"
                                 "SF THE LEFT HAND OF DARKNESS, URSULA K. LE GUIN"
                                 "SF ANATHEM. NEAL STEPHENSON" "SF ARMOR, JOHN STEAKLEY"
                                 "SF ORYX AND CRAKE, MARGARET ATWOOD" "SF FARENHEIT 451, RAY BRADBURY"
                                 "SF A FIRE UPON THE DEEP, VERNOR VINGE" "SF QUARANTINE, GREG EGAN"
                                 "SF THE CHRONICLES OF AMBER, ROGER ZELAZNY")
                      ("* COMSCI BOOKS" "THE PRACTICE OF PROGRAMMING, KERNIGHAN AND PIKE"
                                        "APPRENTICESHIP PATTERNS, DAVE HOOVER (OREILLY OPENBOOK)"
                                        "LINUX DEVICE DRIVERS, 3RD ED, JONATHAN CORBET (OREILLY OPENBOOK)"
                                        "MANAGING PROJECTS WITH GNU MAKE, 3RD ED, ROBERT MECKLENBURG (OREILLY OPENBOOK)"
                                        "REFACTORING: IMPROVING THE DESIGN OF EXISTING CODE"
                                        "SMALLTALK BEST PRACTICE PATTERNS" "SECRETS OF THE JAVASCRIPT NINJA"
                                        "APPLIED CRYPTOGRAPHY, BRUCE SCHNEIER")
                      ("* SITES" "HTTP://HACKERSHELF.COM/BROWSE/"))
                 (sanitize *dummy-file*)))

(define-test parse-entries-test
  (assert-equal '(|* BOOKS|
 (|BEYOND FEAR, BRUCE SCHNEIER| |MOONWALKING WITH EINSTEIN, JOSHUA FOER|
  |SF SNOW CRASH, NEAL STEPHENSON| |SF CHILDHOOD'S END, ARTHUR C. CLARKE|
  |SF DO ANDROIDS DREAM OF ELECTRIC SHEEP, PHILIP K. DICK|
  |SF CONSIDER PHLEBAS, IAIN M. BANKS|
  |SF THE MOTE IN GOD'S EYE, LARRY NIVEN & JERRY POURNELLE|
  |SF THE STARS MY DESTINATION, ALFRED BESTER| |SF CONTACT, CARL SAGAN|
  |SF THE SIRENS OF TITAN, KURT VONNEGUT|
  |SF THE LEFT HAND OF DARKNESS, URSULA K. LE GUIN|
  |SF ANATHEM. NEAL STEPHENSON| |SF ARMOR, JOHN STEAKLEY|
  |SF ORYX AND CRAKE, MARGARET ATWOOD| |SF FARENHEIT 451, RAY BRADBURY|
  |SF A FIRE UPON THE DEEP, VERNOR VINGE| |SF QUARANTINE, GREG EGAN|
  |SF THE CHRONICLES OF AMBER, ROGER ZELAZNY|))
                (parse-entries (list "* BOOKS" "BEYOND FEAR, BRUCE SCHNEIER"
                                 "MOONWALKING WITH EINSTEIN, JOSHUA FOER" "SF SNOW CRASH, NEAL STEPHENSON"
                                 "SF CHILDHOOD'S END, ARTHUR C. CLARKE"
                                 "SF DO ANDROIDS DREAM OF ELECTRIC SHEEP, PHILIP K. DICK"
                                 "SF CONSIDER PHLEBAS, IAIN M. BANKS"
                                 "SF THE MOTE IN GOD'S EYE, LARRY NIVEN & JERRY POURNELLE"
                                 "SF THE STARS MY DESTINATION, ALFRED BESTER" "SF CONTACT, CARL SAGAN"
                                 "SF THE SIRENS OF TITAN, KURT VONNEGUT"
                                 "SF THE LEFT HAND OF DARKNESS, URSULA K. LE GUIN"
                                 "SF ANATHEM. NEAL STEPHENSON" "SF ARMOR, JOHN STEAKLEY"
                                 "SF ORYX AND CRAKE, MARGARET ATWOOD" "SF FARENHEIT 451, RAY BRADBURY"
                                 "SF A FIRE UPON THE DEEP, VERNOR VINGE" "SF QUARANTINE, GREG EGAN"
                                 "SF THE CHRONICLES OF AMBER, ROGER ZELAZNY"))))
