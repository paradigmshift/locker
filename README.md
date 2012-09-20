## locker

A simple program that encrypts (using my "salt-n-pepper" rc4 implementation) and decrypts a file for editing or searching. 

Data structure for the file is a simple associated list, with search being done with the `assoc` function.

Editing the file is done by decrypting the contents of the file, calling an instance of emacs and passing the contents to a buffer called **encrypted**, and once the file has been saved, encrypting the file again with a new passphrase (you can use the old one if you like).

Thanks to Xach, Nikodemus, and H4ns at #lisp for the help in navigating the more *esoteric* aspects of CL.

### Downloading

Code is hosted at [github](https://github.com/paradigmshift/locker), you can clone or for the repo there.

### Installation

Quicklisp's `(ql:quickload "locker")` if the code lives in a location that ASDF2 recognizes and you have Quicklisp.

ASDF's `(asdf:load-system :locker)` if you don't have Quicklisp.

### Exported functions

This is meant to be used by saving a core image file with the code already loaded in. As of the moment, there are **NO** exported functions.

### Command line usage

`locker edit <filename>` - decrypts the file and launches the emacs instance
`locker show <header> <filename>` - searches the filename for matching headers
`locker encrypt <filename>` - encrypts a plaintext file

### Text File instructions

To set a header, put `*` on a blank like followed by a space then header. Everything on the lines next to the header are treated as belonging to it and will show up in a search of the header.

For ex.

    * Coffee
    espresso
    cappuccino

    * Tea
    Earl Grey
    Himalayan

If a search of `locker show coffee <filename>` is entered, the program will return:

    * Coffee
    espresso
    cappuccino

