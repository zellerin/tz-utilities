<a id="x-28TZ-UTILITIES-3A-40INDEX-20MGL-PAX-3ASECTION-29"></a>

# `@INDEX`

## Table of Contents

- [1 Overview][1fd3]
- [2 Access to secrets][7f54]
- [3 Cached variables][51f1]
- [4 Time tools][db65]
- [5 Alist utilities][d4f8]
- [6 Anaphoric macros][1670]
- [7 Named objects][8d1d]
- [8 `@SAVE-LOAD`][fc23]
- [9 `@JS-TOOLS`][fe00]
- [10 `@DEBUGGER-HOOKS`][e333]

###### \[in package TZ-UTILITIES\]
<a id="x-28TZ-UTILITIES-3A-40OVERVIEW-20MGL-PAX-3ASECTION-29"></a>

## 1 Overview

The system contains utilities that I needed in multiple projects that

- I did not find in public repositories and

- are so small that they do not deserve separate package/repository

Not cleaned up and mostly not tested, so probably will not work for you as-is.

<a id="x-28TZ-UTILITIES-3A-40AUTHINFO-20MGL-PAX-3ASECTION-29"></a>

## 2 Access to secrets

Utilities to retrieve passwords from safer storage.

The idea is not to have passwords (and possibly related configuration objects -
names, etc) in the code, but elsewhere, and ask for them when needed in a simple way.

Two backends are in place: [netrc/authinfo][ai] files (internally implemented backed, deprecated), and secret service API, using [SECRET-SERVICE][ss] implementation.

In addition this provides caching of the credentials

[ss]: ../secret-service/ 

[ai]: https://www.gnu.org/software/emacs/manual/html_node/auth/Help-for-users.html 


TODO: is caching still needed/useful?

TODO: Packages - link to documentation or source github?

TODO: Emacs now supports pass - should I as well?

TODO: On multiple matching passwords offer restarts for each (unless there is too much)

<a id="x-28TZ-UTILITIES-3AGET-AUTHINFO-20FUNCTION-29"></a>

- [function] **GET-AUTHINFO** *MACHINE USERNAME*

    Get secret for machine `MACHINE` and login `USERNAME`.
    
    Store already found secrets in a cache and try to retrieve from cache before
    other storate is tried.
    
    Raise condition [`SECRET-NOT-FOUND`][d639] if relevant secret is not found; it has
    established restart [`USE-VALUE`][cf08] that sets the secret in cache as well.

<a id="x-28TZ-UTILITIES-3AGET-AUTHINFO-BOTH-20FUNCTION-29"></a>

- [function] **GET-AUTHINFO-BOTH** *MACHINE*

    Get user and secret for `MACHINE`.
    
    Store already found secrets in a cache and try to retrieve from cache before
    other storage is tried.

<a id="x-28TZ-UTILITIES-3A-2AAUTHINFO-FILES-2A-20VARIABLE-29"></a>

- [variable] **\*AUTHINFO-FILES\*** *("~/.authinfo")*

    Authinfo files to search secrets in.

<a id="x-28TZ-UTILITIES-3ASECRET-NOT-FOUND-20CONDITION-29"></a>

- [condition] **SECRET-NOT-FOUND** *[SERIOUS-CONDITION][af00]*

    Condition thrown when no relevant secret is found.

<a id="x-28TZ-UTILITIES-3A-40CACHED-VARS-20MGL-PAX-3ASECTION-29"></a>

## 3 Cached variables

To prevent long loading times, allow variables to be initialized dynamically -
the initialing form is stored at load time, and evaluated when the form is
used.

The "variable" symbol is expanded to a form "(read-cached symbol)".

Not that the illusion of VAR being really variable breaks when it is used in a
[`LET`][4853] form. This turns out to be quite a serious problem.

<a id="x-28TZ-UTILITIES-3ADEFINE-CACHED-VAR-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-CACHED-VAR** *NAME VALUE &OPTIONAL COMMENT*

    Define `NAME` as a symbol that is evaluated to `VALUE` first time it is used.

<a id="x-28TZ-UTILITIES-3AFORGET-CACHED-20FUNCTION-29"></a>

- [function] **FORGET-CACHED** *VISIBLE-NAME*

    Forget value of cached variable, if it was already evaluated. This causes
    re-evaluation next time it is used.

<a id="x-28TZ-UTILITIES-3A-40TIME-TOOLS-20MGL-PAX-3ASECTION-29"></a>

## 4 Time tools

Time related utilities. The baseline and convention is the [`LOCAL-TIME`][f717] package.

<a id="x-28TZ-UTILITIES-3ATOMORROW-20FUNCTION-29"></a>

- [function] **TOMORROW** *&OPTIONAL (N 1) (BASE (NOW))*

    Timestamp of Nth day in future (1 being tomorrow) 00:00.

<a id="x-28TZ-UTILITIES-3A-2AORG-TIME-FORMAT-2A-20VARIABLE-29"></a>

- [variable] **\*ORG-TIME-FORMAT\*** *(:YEAR "-" (:MONTH 2) "-" (:DAY 2) " " (:HOUR 2) ":" (:MIN 2) " "\
 :SHORT-WEEKDAY)*

    Format of timestamp used by Org mode.

<a id="x-28TZ-UTILITIES-3ALOCAL-TIME-20READTABLE-29"></a>

- [readtable] **LOCAL-TIME**

    @ reads timestring, #@ universal time.

<a id="x-28TZ-UTILITIES-3ADAY-BEFORE-20FUNCTION-29"></a>

- [function] **DAY-BEFORE** *DATE &OPTIONAL (DAYS 1)*

<a id="x-28TZ-UTILITIES-3A-40ALIST-UTILITIES-20MGL-PAX-3ASECTION-29"></a>

## 5 Alist utilities

Add-hoc utilities for alist management.

Three operations on alists when they are used as key-value DB need to be defined:
- getting the value for key or nil (assocd),
- updating value for the key, unconditionally (update-alist)
- getting the value and updating it if it does not exist (get-or-update-alist)

<a id="x-28TZ-UTILITIES-3AASSOCD-20FUNCTION-29"></a>

- [function] **ASSOCD** *ITEM ALIST &REST PARS*

    Convenience shortcut for (cdr (assoc ...)), also known as alist-get in elisp.
    FIXME: this must be in some standard library, but I can't find it.

<a id="x-28TZ-UTILITIES-3AASSOCD-FOR-20FUNCTION-29"></a>

- [function] **ASSOCD-FOR** *KEY &REST PARS*

    Function of one parameter (an alist) that extracts `KEY` from that alist

<a id="x-28TZ-UTILITIES-3ACLIST-TO-LLIST-20FUNCTION-29"></a>

- [function] **CLIST-TO-LLIST** *OBJECT*

    Convert list of conses (e.g., used by hunchentoot) to list of lists
    (e.g., used for display in org mode).

<a id="x-28TZ-UTILITIES-3AGET-OR-UPDATE-ALIST-20MGL-PAX-3AMACRO-29"></a>

- [macro] **GET-OR-UPDATE-ALIST** *(ALIST KEY &REST ASSOC-PARS &KEY &ALLOW-OTHER-KEYS) &BODY NEW-VAL-CODE*

    Get value associated with `KEY` in the `ALIST`; if it is not present,
    update `ALIST` with `KEY` and evaluate `NEW-VAL-CODE` as new `VALUE`.

<a id="x-28TZ-UTILITIES-3AWITH-SMALL-CACHE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **WITH-SMALL-CACHE** *(KEY &REST PARS-TEST) &BODY BODY*

    Return body, or it cached value. Caching is done on current and
    cached `KEY`s being same; keyword parameters `KEY` and `TEST` can modify
    what same means.
    
    The cache is implemented as an alist, so should be small to keep efficiency.

<a id="x-28TZ-UTILITIES-3APRUNE-ALIST-20FUNCTION-29"></a>

- [function] **PRUNE-ALIST** *ALIST TEST*

    Remove duplicate entries from an alist.

<a id="x-28TZ-UTILITIES-3AUPDATE-ALIST-20MGL-PAX-3AMACRO-29"></a>

- [macro] **UPDATE-ALIST** *PLACE KEY NEW-VALUE &REST TEST-PARS*

    Set value for `KEY` in modified alist to `NEW-VALUE`

<a id="x-28TZ-UTILITIES-3A-40ANAPHORIC-20MGL-PAX-3ASECTION-29"></a>

## 6 Anaphoric macros

Anaphoric macros not defined in the Let-over-lambda package.

<a id="x-28TZ-UTILITIES-3AAWHEN-20MGL-PAX-3AMACRO-29"></a>

- [macro] **AWHEN** *TEST-FORM &BODY BODY*

    Evaluate `TEST-FORM`, and if true, evaluate `BODY` with symbol [`IT`][cca7] bound to its
    value.

<a id="x-28LET-OVER-LAMBDA-3AIT-20VARIABLE-29"></a>

- [variable] **IT**

<a id="x-28TZ-UTILITIES-3A-40NAMED-OBJECTS-20MGL-PAX-3ASECTION-29"></a>

## 7 Named objects

Class [`NAMED`][9f89] streamlines creating "printable" named class instances.

Class [`NAMED-JSON`][6663] allows easier handling and pretty-printing of alist
objects. Word `JSON` in name is probably a bad one, but this is what I use it for:
alists obtained from cl-json:decode-json calls

<a id="x-28TZ-UTILITIES-3ANAMED-20CLASS-29"></a>

- [class] **NAMED**

    Use as a mixin/base class to ensure that the class has slot `NAME`. This slot is
    used to pretty-print the objects.

<a id="x-28TZ-UTILITIES-3AGET-NAME-20GENERIC-FUNCTION-29"></a>

- [generic-function] **GET-NAME** *OBJECT*

    Name of the object. If it is named object, value of the `NAME` slot, otherwise
    object itself (suitable for strings, numbers, etc.)

<a id="x-28TZ-UTILITIES-3ANO-NAME-20CONDITION-29"></a>

- [condition] **NO-NAME** *[SERIOUS-CONDITION][af00]*

    Condition thrown when an object of class [`NAMED`][9f89] has no `NAME` specified at
    creation.

<a id="x-28TZ-UTILITIES-3ANAMED-JSON-20CLASS-29"></a>

- [class] **NAMED-JSON** *[NAMED][9f89]*

    Objects backed up/created from an alist structure (e.g., decoded json) that
    contains, among other, name/display name of the object.

<a id="x-28TZ-UTILITIES-3AMAKE-JSON-OBJECT-20FUNCTION-29"></a>

- [function] **MAKE-JSON-OBJECT** *JSON CLASS &REST PARS &KEY (NAME-KEYWORD :NAME) (NAME-FN (ASSOCD-FOR NAME-KEYWORD)) &ALLOW-OTHER-KEYS*

    Make instance of an object backed by an alist. Typically, sets name based on
    the value associated with `NAME-KEYWORD`, but another function to extract name can
    be specified with NAMED-FN.

<a id="x-28TZ-UTILITIES-3AMAKE-JSON-OBJECTS-20FUNCTION-29"></a>

- [function] **MAKE-JSON-OBJECTS** *LIST-OF-JSON &REST PARS*

<a id="x-28TZ-UTILITIES-3A-40SAVE-LOAD-20MGL-PAX-3ASECTION-29"></a>

## 8 `@SAVE-LOAD`

To preserve relatively stable data across run of the system, the
actual values of the variables can be saved and then loaded on
startup (or anytime else).

The data are saved to a define directory with names created from the
provided string and timestamp; loading is done from the most recent
(based on name) file.

The background mechanism for storing is cl-store.

Typical sequence is
`
(defvar *A-VARIABLE* (load-value "foo"))
...
;;; some long calculation to update *A-VARIABLE*
...
(save-value *A-VARIABLE* "foo")
`

<a id="x-28TZ-UTILITIES-3ASAVE-VALUE-20FUNCTION-29"></a>

- [function] **SAVE-VALUE** *VALUE BASE-NAME &KEY (CACHE-PATH \*DEFAULT-CACHE-PATH\*)*

    Store value to a timestamped storage in cache.

<a id="x-28TZ-UTILITIES-3ALOAD-VALUE-20FUNCTION-29"></a>

- [function] **LOAD-VALUE** *BASE-NAME &KEY (CACHE-PATH \*DEFAULT-CACHE-PATH\*)*

    Load value keyed by `BASE-NAME` from the most recent file (by name) in cache.

<a id="x-28TZ-UTILITIES-3A-2ADEFAULT-CACHE-PATH-2A-20VARIABLE-29"></a>

- [variable] **\*DEFAULT-CACHE-PATH\*** *"~/.cache/lisp/"*

    Default directory to save the variable values.

<a id="x-28TZ-UTILITIES-3A-40JS-TOOLS-20MGL-PAX-3ASECTION-29"></a>

## 9 `@JS-TOOLS`

Function on lists that facilitate work with json data after parsing
them with cl-json. See also LET-ALIST.

<a id="x-28TZ-UTILITIES-3AEXTRACT-TAGS-20FUNCTION-29"></a>

- [function] **EXTRACT-TAGS** *ALIST TAG-SPECS*

    Extracts values in the `ALIST` matching keys in `TAG-SPECS`, optionally applying a
    function specified in `TAG-SPECS`.
    
    `TAG-SPECS` is a list where each element is either a `KEY` or list (`KEY` FN).
    
    Example:
    : (extract-tags '((:foo . 1) (:bar 2) (:baz 3)) '(:baz (:foo 1-)))
    gives
    : ((3) 0)
    
    Maybe LET-ALIST is more natural interface in many cases.
    
    Not optimized for speed:
    - iterates tags-specs twice
    - conses a lot
    - etc.

<a id="x-28TZ-UTILITIES-3AEXTRACT-TAGS-FROM-LIST-20FUNCTION-29"></a>

- [function] **EXTRACT-TAGS-FROM-LIST** *TAGS DATA*

    [`EXTRACT-TAGS`][4156] on each item in list `DATA`.

<a id="x-28TZ-UTILITIES-3AFILL-TEMPLATE-20FUNCTION-29"></a>

- [function] **FILL-TEMPLATE** *TEMPLATE KEYS-VALUES*

    Fill a template suitable for json with values.
    
    Return a copy of template with each cons that starts with a `KEY` from
    `KEYS-VALUES` plist has its cdr replaced with the appropriate `VALUE`.
    
    Example:
    : (fill-template '(Here is a (:value "To be filled"))  ((:value 42)))
    gives
    : (HERE IS A (`:VALUE` . 42))

<a id="x-28TZ-UTILITIES-3AFILL-TEMPLATE-2A-20FUNCTION-29"></a>

- [function] **FILL-TEMPLATE\*** *TEMPLATE &REST KEYS-VALUES*

    Same as [`FILL-TEMPLATE`][ccaf], but with `KEYS-VALUES` as call parameters.

<a id="x-28TZ-UTILITIES-3AJSON-BASED-SIMPLE-20CLASS-29"></a>

- [class] **JSON-BASED-SIMPLE**

    A class that can be initiated from `JSON` data. There are two additional initargs,
    :slot-map and :json, that maps json path and slot name. Each of keywords in SLOT-MAP is
    sought by `ALIST` in the `JSON` and if present, both are added to the initargs.
    
    The `JSON` can be also a slot, of course. And `:SLOT-MAP` can be in the default-initargs.
    
    Example: define a derived class,
    : (defclass json-example (json-based-simple)
    :  ((foo :accessor get-foo :initarg :foo)
    :   (bar :accessor get-bar :initarg :bar)))
    
    and then use
    : (make-instance 'json-example :slot-map
    :      '(:foo :bar) :json '(:foo 12))

<a id="x-28TZ-UTILITIES-3A-40DEBUGGER-HOOKS-20MGL-PAX-3ASECTION-29"></a>

## 10 `@DEBUGGER-HOOKS`

[`MAKE-CASCADED-DEBUGGER-HOOK`][8e0f] is used to add additional handler cases for current repl.

This is probably not good idea to use and fully experimental.

<a id="x-28TZ-UTILITIES-3AMAKE-CASCADED-DEBUGGER-HOOK-20MGL-PAX-3AMACRO-29"></a>

- [macro] **MAKE-CASCADED-DEBUGGER-HOOK** *CASES*

    A function suitable for the debugger hook that checks `CASES` (same format as in
     handler-case) and if none matches, runs whatever was the hook when it was called.
    
    Usage:
    : (setq *debugger-hook*
    :    (make-cascaded-debugger-hook '((simple-error continue)))))

  [1670]: #x-28TZ-UTILITIES-3A-40ANAPHORIC-20MGL-PAX-3ASECTION-29 "Anaphoric macros"
  [1fd3]: #x-28TZ-UTILITIES-3A-40OVERVIEW-20MGL-PAX-3ASECTION-29 "Overview"
  [4156]: #x-28TZ-UTILITIES-3AEXTRACT-TAGS-20FUNCTION-29 "TZ-UTILITIES:EXTRACT-TAGS FUNCTION"
  [4853]: http://www.lispworks.com/documentation/HyperSpec/Body/s_let_l.htm "LET (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [51f1]: #x-28TZ-UTILITIES-3A-40CACHED-VARS-20MGL-PAX-3ASECTION-29 "Cached variables"
  [6663]: #x-28TZ-UTILITIES-3ANAMED-JSON-20CLASS-29 "TZ-UTILITIES:NAMED-JSON CLASS"
  [7f54]: #x-28TZ-UTILITIES-3A-40AUTHINFO-20MGL-PAX-3ASECTION-29 "Access to secrets"
  [8d1d]: #x-28TZ-UTILITIES-3A-40NAMED-OBJECTS-20MGL-PAX-3ASECTION-29 "Named objects"
  [8e0f]: #x-28TZ-UTILITIES-3AMAKE-CASCADED-DEBUGGER-HOOK-20MGL-PAX-3AMACRO-29 "TZ-UTILITIES:MAKE-CASCADED-DEBUGGER-HOOK MGL-PAX:MACRO"
  [9f89]: #x-28TZ-UTILITIES-3ANAMED-20CLASS-29 "TZ-UTILITIES:NAMED CLASS"
  [af00]: http://www.lispworks.com/documentation/HyperSpec/Body/e_seriou.htm "SERIOUS-CONDITION (MGL-PAX:CLHS CONDITION)"
  [cca7]: #x-28LET-OVER-LAMBDA-3AIT-20VARIABLE-29 "LET-OVER-LAMBDA:IT VARIABLE"
  [ccaf]: #x-28TZ-UTILITIES-3AFILL-TEMPLATE-20FUNCTION-29 "TZ-UTILITIES:FILL-TEMPLATE FUNCTION"
  [cf08]: http://www.lispworks.com/documentation/HyperSpec/Body/r_use_va.htm "USE-VALUE (MGL-PAX:CLHS RESTART)"
  [d4f8]: #x-28TZ-UTILITIES-3A-40ALIST-UTILITIES-20MGL-PAX-3ASECTION-29 "Alist utilities"
  [d639]: #x-28TZ-UTILITIES-3ASECRET-NOT-FOUND-20CONDITION-29 "TZ-UTILITIES:SECRET-NOT-FOUND CONDITION"
  [db65]: #x-28TZ-UTILITIES-3A-40TIME-TOOLS-20MGL-PAX-3ASECTION-29 "Time tools"
  [e333]: #x-28TZ-UTILITIES-3A-40DEBUGGER-HOOKS-20MGL-PAX-3ASECTION-29 "`TZ-UTILITIES::@DEBUGGER-HOOKS`"
  [f717]: #x-28TZ-UTILITIES-3ALOCAL-TIME-20READTABLE-29 "TZ-UTILITIES:LOCAL-TIME READTABLE"
  [fc23]: #x-28TZ-UTILITIES-3A-40SAVE-LOAD-20MGL-PAX-3ASECTION-29 "`TZ-UTILITIES::@SAVE-LOAD`"
  [fe00]: #x-28TZ-UTILITIES-3A-40JS-TOOLS-20MGL-PAX-3ASECTION-29 "`TZ-UTILITIES::@JS-TOOLS`"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
