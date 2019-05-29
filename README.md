# Emacs CheatSheet

## To get Emacs Running Faster

`emacsclient -a "" -c`

```elisp
`C-h` ;;Help
`C-x C-c` ;;Exit
`C-x C-f` ;;Find File
`C-x C-s` ;;Save the file
`C-h t ;;Built-in tutorial
`C-p` ;;Previous line.
`C-n` ;;Next line.
`C-b` ;;Backward one character.
`C-f` ;;Forward one character.
`M-f` ;;Forward one word.
`M-b` ;;Backward one word.
ctrl+shift+backspace ;;delete the line
C-x C-+/ C-x C--(thats' ctrl minus) ;;Change font size
```
`M-x M-p` to cycle through previous commands in `M-x`


```
Keys 	Description
`C-a` 	Move to beginning of line.
`M-m` 	Move to first non-whitespace character on the line.
`C-e` 	Move to end of line.
`C-f` 	Move forward one character.
`C-b` 	Move backward one character.
`M-f` 	Move forward one word (I use this a lot).
`M-b` 	Move backward one word (I use this a lot, too).
`C-s` 	Regex search for text in current buffer and move to it. Press C-s again to move to next match.
`C-r` 	Same as C-s, but search in reverse.
`M-<` 	Move to beginning of buffer.
`M->` 	Move to end of buffer.
`M-g g` 	Go to line. 
```



```
Keys 	Description
`C-w` 	Kill region.
`M-w` 	Copy region to kill ring.
`C-y` 	Yank.
`M-y` 	Cycle through kill ring after yanking.
`M-d` 	Kill word.
`C-k` 	Kill line. 
```

```

Keys 	Description
Tab 	Indent line.
`C-j` 	New line and indent, equivalent to enter followed by tab.
`M-/` 	Hippie expand; cycles through possible expansions of the text before point.
`M-\` 	Delete all spaces and tabs around point. (I use this one a lot.)
```


```

Keys 	Description
`C-x o` 	Switch cursor to another window. Try this now to switch between your Clojure file and the REPL.
`C-x 1` 	Delete all other windows, leaving only the current window in the frame. This doesn’t close your buffers, and it won’t cause you to lose any work.
`C-x 2` 	Split frame above and below.
`C-x 3` 	Split frame side by side.
`C-x 0` 	Delete current window. 
```



### Movement



```
`C-n` Next line
`C-p` Previous line
`C-f` Move cursor forward one character
`C-b` Move cursor backward one character
`M-f` Forward one word
`M-b` Backward one word
`C-a` Move to beginning of line
`C-e` Move to end of line
`M-a` Move back to beginning of the sentence
`M-e` Move back to end of the sentence
`M-<` Go to the beginning of the file
`M->` Go to the end of the file
`M-g` g N Go to line N

```

To perform this function             | Use these keys
-------------------------------------|---------------------------------
Moving to the next line              | C-n (n for Next)
Moving to the previous line          | C-p (p for Previous)
Moving one character forward         | C-f (f for Forward) 
Moving one character backward        | C-b (b for Backward)
Moving one word forward              | M-f (f for Forward) 
Moving one word backward             | M-b (b for Backward)
Moving to the start of a line        | C-a 
Moving to the end of a line          | C-e (e for End)
Moving to the start of a sentence    | M-a
Moving to the end of a sentence      | M-e (e for End)
Moving one page down                 | C-v (or PgDn)
Moving one page up                   | M-v (or PgUp)
Moving to the beginning of the file  | M-< (Alt + Shift + "<")
Moving to the end of the file        | M-> (Alt + Shift + ">")

### Searching

`C-s` Start a forward search.
`C-r` Start a reverse search.

### Deleting Text

`C-d` Delete a character
`M-d` Delete a word

### Copy/Paste

`C-k` Kill (cut) the rest of the current line of text
`C-y` Yank (or paste) from the previously killed text

### Undo

`C-x u` Undo.
`C-_` Undo.
`C-/` Undo.

### Redo
`C-S-/` Redo. [ctrl+shift+/]

### Save and Quit

`C-x C-s` Save the file
`C-x C-c` Exit

### Help

C-h Help
C-h t Tutorial
C-h k {key} Describe {key}

### Find and Replace

M-% {old} {new} ! Substitute {new} for {old} in the entire document

### Repeating Commands

C-u {num} {command} Repeat {command} {num} times

### Legend

```
M-x means “press and release the Escape key and then press the x key. “
C-x means “press and hold the Control key and then press and release the x key.”
C-<char> Ctrl while pressing <char>.
M-<char> "Meta" key (alt key) while pressing <char>.
M-<char> Esc, then type <char>

```
### To relaod Buffer

M-x revert-buffer


```
key 	what it does
`C-x 2` 	split-window-below (vertically)
`C-x 3` 	split-window-right (horizontally)
`C-x 0` 	delete-window (this one)
`C-x 1` 	delete-other-windows
`C-x o` 	other-window (moves foxus to the next window

```

###   In Org-Mode to Write Code

`C-c '`  This will write Code in a new window 

###  https://emacs.stackexchange.com/questions/40571/how-to-set-a-short-cut-for-begin-src-end-src/47370#47370

## Easy templates

With just a few keystrokes, Org’s easy templates inserts empty pairs of structural elements, such as #+BEGIN_SRC and #+END_SRC. Easy templates use an expansion mechanism, which is native to Org, in a process similar to yasnippet and other Emacs template expansion packages.

< s TAB expands to a ‘src’ code block.

< l TAB expands to:

#+BEGIN_EXPORT latex

#+END_EXPORT

```
Org comes with these pre-defined easy templates:
s	#+BEGIN_SRC ... #+END_SRC
e	#+BEGIN_EXAMPLE ... #+END_EXAMPLE
q	#+BEGIN_QUOTE ... #+END_QUOTE
v	#+BEGIN_VERSE ... #+END_VERSE
c	#+BEGIN_CENTER ... #+END_CENTER
C	#+BEGIN_COMMENT ... #+END_COMMENT
l	#+BEGIN_EXPORT latex ... #+END_EXPORT
L	#+LATEX:
h	#+BEGIN_EXPORT html ... #+END_EXPORT
H	#+HTML:
a	#+BEGIN_EXPORT ascii ... #+END_EXPORT
A	#+ASCII:
i	#+INDEX: line
I	#+INCLUDE: line

```
More templates can added by customizing the variable org-structure-template-alist, whose docstring has additional details. 


## Macro's

Command 	Key
`kmacro-start-macro-or-insert-counter` 	`F3` or `C-x (`
`kmacro-end-and-call-macro` 	`F4` or `C-x )`
`kmocro-call-marco`       `C-x e`
`kmacro-name-last-macro` 	
`insert-kbd-macro` 	

## Marks

`C-<SPC>` Set the mark at point, and activate it (set-mark-command).
`C-@`   The same. 

## Case Conversion Commands

Emacs has commands for converting either a single word or any arbitrary range of text to upper case or to lower case.

`M-l` Convert following word to lower case (downcase-word).
`M-u` Convert following word to upper case (upcase-word).
`M-c` Capitalize the following word (capitalize-word).
`C-x C-l` Convert region to lower case (downcase-region).
`C-x C-u` Convert region to upper case (upcase-region). 

### I-edit

`C-;` mark all the marked words
