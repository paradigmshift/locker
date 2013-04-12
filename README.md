## locker

A simple program that encrypts (using my "salt-n-pepper" rc4 implementation) and decrypts a file for editing or searching. 

Data structure for the file is a simple associated list, with search being done with the `assoc` function.

Editing the file is done by decrypting the contents of the file, calling an instance of emacs and passing the contents to a buffer called **encrypted**, and once the file has been saved, encrypting the file again with a new passphrase (you can use the old one if you like).

Thanks to Xach, Nikodemus, and H4ns at #lisp for the help in navigating the more *esoteric* aspects of CL.

### License

FreeBSD License

Copyright (c) 2013, Mozart Reina
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

### Downloading

Code is hosted at [github](https://github.com/paradigmshift/locker), you can clone or fork the repo from there.

### Installation

Quicklisp's `(ql:quickload "locker")` if the code lives in a location that ASDF2 recognizes and you have Quicklisp.

ASDF's `(asdf:load-system :locker)` if you don't have Quicklisp.

### OS X bundle for the GUI

The way I created a Finder-clickable bundle for OS X without launching the terminal first:

Create a script with the following contents:
`open -a <path-to-gui-executable>`

Download [Platypus](http://sveinbjorn.org/platypus) and have it create a bundle from the script you just wrote.

Use XCode to add `LSEnvironment` and set `LANG` to `en_US.UTF-8`  in the `info.plist` file.

You should now have a clickable app.

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

