## locker

Desktop/CLI application for encrypting files that contain lists, uses headers denoted by `*` to mark categories. GUI uses TK bindings through LTK.

### Languages
- Common Lisp

### Dependencies
- SBCL
- [Salt-n-Pepper](https://github.com/paradigmshift/salt-n-pepper)
- [LTK](http://www.peter-herth.de/ltk)

### Functions

*function* **sb-alien:define-alien-routine & pass-prompt**

Call to the Getpass library written in C. Used for accepting a passphrase without echoing the characters entered.
********************
*function* **open-file**
**open-file** *filename, t|nil -> contents of the file*

If the decode flag is set to **t**, a passphrase is prompted to decrypt the file and display its contents.
********************
*function* **split-header**

**split-header** *string -> list with string elements*

Elements of the string returned are delineated by `*`, which mark a header.

Example:

`*Coffee espresso cappuccino *Tea Earl Grey Himalayan` -> `("*Coffee espresso cappuccino" "*Tea Earl Grey Himalayan")`
********************
*function* **split-newline**

**split-newline** *string -> list with string elements*

Same as `split-header` but splits the string by newlines instead of headers.
********************
*function* **split-space**

**split-space** *string -> list with string elements*

Same as `split-header` but splits the string by spaces instead of headers.
********************
*function* **sanitize**

**sanitize** *string -> list with list and string elements*

Wraps `split-header`, `split-newline`, `remove-empty-entries`, and `remove-empty-lst`.
********************
*function* **parse-entries**

**parse-entries** *list with string elements -> alist*

Takes the list and parses the strings, strings starting with `*` are relegated as the key, and everything else is the value. Keys are converted to symbols to ease manipulation.
********************
*function* **access**

**access** *query-string, alist -> list*

Takes a query and an associated list, converts the query to a symbol and attempts to match it with the alist.
********************
*function* **edit-file**

**edit-file** *filename*

Decrypts a file with a given passhphrase, sends the contents to an emacs instance, then encrypts the file after emacs is exited. The file must first be saved in emacs for the changes to take effect.
********************
*function* **toplevel**

Function for parsing command line arguments and calling the respective internal functions.
********************