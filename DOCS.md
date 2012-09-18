## locker

### sb-alien:define-alien-routine & pass-prompt
Call to the Getpass library written in C. Used for accepting a passphrase without echoing the characters entered.

### open-file *function*
*filename, t or nil -> contents of the file*

If the decode flag is set to **t**, a passphrase is prompted to decrypt the file and display its contents.

Open-file contains two routines for reading files, one encoded and one unencoded. This is due to my, undoubtedly, inexperience and poor understanding of how to files are written, read, and how the different macros affect them. 

### split-header *function*
*string -> list with string elements*

Elements of the string returned are delineated by `*`, which mark a header.

Example:

"*Coffee espresso cappuccino *Tea Earl Grey Himalayan" -> "("*Coffee espresso cappuccino" "*Tea Earl Grey Himalayan")

### split-newline *function*
*string -> list with string elements*

Same as `split-header` but splits the string by newlines instead of headers.

### split-space *function*
*string -> list with string elements*

Same as `split-header` but splits the string by spaces instead of headers.

### sanitize *function*
*string -> list with list and string elements*

Wraps `split-header`, `split-newline`, `remove-empty-entries`, and `remove-empty-lst`.

### parse-entries *function*
*list with string elements -> alist*

Takes the list and parses the strings, strings starting with `*` are relegated as the key, and everything else is the value. Keys are converted to symbols to ease manipulation.

### access *function*
*query-string, alist -> list*

Takes a query and an associated list, converts the query to a symbol and attempts to match it with the alist.


### edit-file *function*
*filename*

Decrypts a file with a given passhphrase, sends the contents to an emacs instance, then encrypts the file after emacs is exited. The file must first be saved in emacs for the changes to take effect.

### toplevel *function*

Function for parsing command line arguments and calling the respective internal functions.