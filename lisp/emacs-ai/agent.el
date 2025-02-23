(require 'gptel)
(require 'cl-lib)
(defvar agent-tools nil
  "Default provided tools.")

(setf agent-tools nil)
(defmacro define-agent-tool (name function args description)
  "Define and push a new agent tool to `agent-tools`."
  `(push (gptel-make-tool
          :function ,function
          :name ,name
          :args ,args
          :description ,description)
         agent-tools))

(defun agent--file-exists (filename)
  "Check if FILENAME exists and return JSON boolean string.
Handles path expansion including ~/ for home directory."
  (if (file-exists-p (expand-file-name filename))
      "true"
    "false"))

(defun agent--read-file (filename)
  "Read contents of FILENAME and return as string.
Handles path expansion including ~/ for home directory."
  (if (file-exists-p (expand-file-name filename))
      (with-temp-buffer
        (insert-file-contents (expand-file-name filename))
        (buffer-string))
    (error "File not found: %s" filename)))

(define-agent-tool
 "file_exists"
 #'agent--file-exists
 '((:name "filename"
    :type string
    :description "Path to file to check for existence"))
 "Check if a file exists. Returns 'true' if the file exists, 'false' otherwise.
If the tool result fails, try correcting the spelling or casing.")

(define-agent-tool
 "read_file"
 #'agent--read-file
 '((:name "filename"
    :type string
    :description "Path to file to read. Can use '~/' for home directory expansion."))
 "Read and return the contents of a file as a string.
Before using this tool:
1. ALWAYS check if the file exists first using the file_exists tool.
2. If file_exists returns false, try correcting the path/spelling.
3. Only attempt to read after confirming existence.
4. Once you read it, only read it again if the user indicates it changed or you modified it.
Returns the file contents as a string or throws an error if the file is not found.")

(defun agent--write-file (filename content &optional concat)
  "Write CONTENT to FILENAME with optional merging.
- If file exists and MERGE is non-nil, append or merge content.
- If file doesn't exist, create and write the content.
- Handles path expansion including ~/ for home directory.
Returns a JSON-compatible success message."
  (let* ((expanded-filename (expand-file-name filename))
         (existing-content (when (file-exists-p expanded-filename)
                             (with-temp-buffer
                               (insert-file-contents expanded-filename)
                               (buffer-string)))))
    (with-temp-file expanded-filename
      (insert (if (and concat existing-content)
                  (concat existing-content "\n" content)
                content)))
    "File written successfully"))

(define-agent-tool
 "write_file"
 #'agent--write-file
 '((:name "filename"
    :type string
    :description "Path to file to write. Can use '~/' for home directory expansion.")
   (:name "content"
    :type string
    :description "Content to write to the file.")
   (:name "concat"
    :type boolean
    :description "Optional. If true and file exists, append content to existing file.
If false or not provided, overwrite the existing file. MAKE SURE IF YOU USE THIS YOUR CONTENT MATCHES THE OLD CHECKED CONTENT"))
 "Write content to a file with optional merging.
Key behaviors:
1. ALWAYS check file existence before writing
2. Supports creating new files
3. Optional concat
4. Handles path expansion
5. Provides clear success feedback

Example use cases:
- Create a new log file
- Append to an existing configuration
- Save project notes
- Update tracking files

Recommended workflow:
1. Use file_exists to check file status
2. If the user has stated to be safe first use file_backup
3. Decide merge strategy (merge, do not use concat) (concat: read the file first (just once) and then concat)
4. Call write_file with appropriate parameters")



(defun agent--file-backup (filename)
  "Create a comprehensive backup of FILENAME.
- Copies file to filename.bak
- Handles non-existent files gracefully
- Provides verbose backup tracking"
  (when (file-exists-p filename)
    (copy-file filename (concat filename ".bak") t)
    (format "Backup created: %s.bak" filename)))

(define-agent-tool
 "file_backup"
 #'agent--file-backup
 '((:name "filename"
    :type string
    :description "Path to file to backup. Must be an existing file."))
 "Create a safe backup of a file by copying it to a .bak extension.

Key Safety Features:
- Preserves original file integrity
- Creates timestamped backup copy
- Supports full path and ~ home directory expansion
- Prevents accidental data loss during modifications

Recommended Workflow:
1. Use file_exists to verify source file
2. Call file_backup before critical modifications
3. Maintain a version history of important files

Use Cases:
- Protecting configuration files
- Versioning documents
- Preparing for risky file transformations

Backup Strategy:
- Simple .bak extension (filename.bak)
- Overwrites previous backup
- No limit on backup generations (user-managed)

Safety Tip: Always verify backup before destructive operations.")


(define-agent-tool
 "list_files"
 (lambda (path)
   (let* ((dir (if (or (null path) (string= path ""))
                   default-directory
                 (expand-file-name path))))
     (if (file-directory-p dir)
         (directory-files dir nil "^[^.].*")
       (error "Not a valid directory: %s" dir))))
 '((:name "path"
    :type string
    :description "Directory path to list files from. Supports:
- Absolute paths (e.g., '/home/user/documents')
- Relative paths (e.g., './projects', '../downloads')
- Home directory expansion with '~/' (e.g., '~/Documents')
- Handles both existing and potentially non-existent directories
- Resolves symbolic links and canonical paths
- Respects user's file system permissions
Optional trailing slash is accepted.
Defaults to current working directory if not specified."))
 "List files in the specified directory.
Returns a list of file names, excluding '.' and '..'.")

(define-agent-tool
 "list_files_recursive"
 (lambda (path)
   (let* ((dir (if (or (null path) (string= path ""))
                   default-directory
                 (expand-file-name path))))
     (if (file-directory-p dir)
         (directory-files-recursively dir "^[^.].*")
       (error "Not a valid directory: %s" dir))))
 '((:name "path"
    :type string
    :description "Directory path to recursively list files from. Supports:
- Absolute paths (e.g., '/home/user/documents')
- Relative paths (e.g., './projects', '../downloads')
- Home directory expansion with '~/' (e.g., '~/Documents')
- Handles both existing and potentially non-existent directories
- Resolves symbolic links and canonical paths
- Respects user's file system permissions
Optional trailing slash is accepted.
Defaults to current working directory if not specified."))
 "Recursively list files in the specified directory.
Returns a list of file names, excluding '.' and '..'.")

(setf gptel-tools nil)
(setq gptel-tools (cl-remove-duplicates agent-tools

                                        (defun pmx--gptel-symbolp (name)
                                          (intern-soft name))

                                        (defun pmx--gptel-manual-names ()
                                          (json-serialize (vconcat (info--filter-manual-names
                                                                    (info--manual-names nil)))))

                                        (defun pmx--gptel-manual-list-nodes (name)
                                          (json-serialize
                                           (vconcat
                                            (mapcar #'car (Info-build-node-completions name)))))

                                        (defun pmx--gptel-manual-node-contents (manual node)
                                          (condition-case err
                                              (progn
                                                (save-window-excursion
                                                  (Info-goto-node (format "(%s)%s" manual node))
                                                  (buffer-substring-no-properties (point-min) (point-max))))
                                            (user-error
                                             (error (error-message-string err)))))

                                        (defun pmx--gptel-symbol-in-manual (symbol)
                                          (require 'helpful)
                                          (when-let* ((symbol (intern-soft symbol))
                                                      (_completion (helpful--in-manual-p symbol)))
                                            (save-window-excursion
                                              (info-lookup 'symbol symbol #'emacs-lisp-mode)
                                              (buffer-substring-no-properties (point-min) (point-max)))))

                                        (defun pmx--gptel-featurep (feature)
                                          "Return non-nil if FEATURE is loaded or available.
User might not have FEATURE loaded if it is an autoload etc."
                                          (if-let ((feature-symbol (intern-soft feature)))
                                              (when (featurep feature-symbol)
                                                feature)
                                            (find-library-name feature)))

                                        (defun pmx--gptel-features ()
                                          (mapconcat #'symbol-name features "\n"))

                                        (defun pmx--gptel-load-paths ()
                                          (string-join load-path "\n"))

                                        (defun pmx--gptel-library-source (library-name)
                                          "Return the source code of LIBRARY-NAME as a string."
                                          (if-let ((library (find-library-name library-name)))
                                              (with-temp-buffer
                                                (progn
                                                  (insert-file-contents library)
                                                  (buffer-string)))
                                            (error "Library not found: %s" library-name)))

                                        (defun pmx--gptel-source (symbol &optional type)
                                          "Retrieve the source code for SYMBOL of TYPE.
SYMBOL should be a function or variable name, given as a string or symbol.
TYPE can be nil for functions, 'defvar for variables, or 'defface for faces.
Returns the source code as a string, or nil if the definition is not found."
                                          (when-let* ((callable (intern-soft symbol))
                                                      (save-silently t)         ; suppresses message in
                                        ; find-file-noselect
                                                      (vc-follow-symlinks t)     ; don't ask, we're not editing.
                                                      (buffer-point (find-definition-noselect callable type)))
                                            (with-current-buffer (car buffer-point)
                                              (goto-char (cdr buffer-point))
                                              (buffer-substring-no-properties
                                               (point)
                                               (progn (if (null type)
                                                          (end-of-defun)
                                                        (cond ((derived-mode-p 'c-mode)
                                                               (forward-sexp 2)
                                                               (forward-char))
                                                              ((derived-mode-p 'emacs-lisp-mode)
                                                               (forward-sexp))
                                                              (t (error "Unexpected file mode"))))
                                                      (point))))))

                                        (defun pmx--gptel-function-completions (prefix)
                                          (require 'orderless)
                                          (string-join (orderless-filter prefix obarray #'functionp) "\n"))

                                        (defun pmx--gptel-command-completions (prefix)
                                          (require 'orderless)
                                          (string-join (orderless-filter prefix obarray #'commandp) "\n"))

                                        (defun pmx--gptel-variable-completions (prefix)
                                          (require 'orderless)
                                          (string-join (orderless-filter prefix obarray #'boundp) "\n"))

                                        (defun pmx--gptel-function-source (symbol)
                                          (when-let ((symbol (intern-soft symbol)))
                                            (pmx--gptel-source symbol)))

                                        (defun pmx--gptel-variable-source (symbol)
                                          (when-let ((symbol (intern-soft symbol)))
                                            (pmx--gptel-source symbol 'defvar)))

                                        (defun pmx--gptel-function-documentation (symbol)
                                          (when-let ((symbol (intern-soft symbol)))
                                            (documentation symbol)))

                                        (defun pmx--gptel-variable-documentation (symbol)
                                          (when-let ((symbol (intern-soft symbol)))
                                            (custom-variable-documentation symbol)))

                                        (defun pmx--gptel-variable-global-value (symbol)
                                          (when-let ((symbol (intern-soft symbol)))
                                            (default-value symbol)))

                                        (defun pmx--gptel-eval (expression)
                                          (format "%S" (eval (read expression))))

                                        (defun pmx--gptel-simulate-error ()
                                          (error "This is a simulated error message.  OMGWTF."))

                                        (defun pmx--gptel-coerce-nil ()
                                          nil)

                                        (defun pmx--gptel-all-arg-types (object string array null true false enum)
                                          (message "object: %S\nstring: %S\narray: %S\nnull: %S\ntrue: %S\nfalse: %S\n\
enum: %S"
                                                   object string array null true false enum))

                                        (defun pmx--gptel-async-tool (callback later-val)
                                          (sit-for 2)
                                          (funcall callback (format "Do it %s." later-val)))

                                        (list
                                         (gptel-make-tool
                                          :function #'pmx--gptel-eval
                                          :name "elisp_eval"
                                          :confirm t
                                          :include t
                                          :category "introspection"
                                          :args '(( :name "expression"
                                                    :type string
                                                    :description "A single elisp sexp to evaluate."))
                                          :description "Evaluate Elisp EXPRESSION and return result.
EXPRESSION can be anything will evaluate.  It can be a function call, a
variable, a quasi-quoted expression.  The only requirement is that only
the first sexp will be read and evaluated, so if you need to evaluate
multiple expressions, make one call per expression.  Do not combine
expressions using progn etc.  Just go expression by expression and try
to make standalone single expressions.

Instead of saying \"I can't calculate that\" etc, use this tool to
evaluate the result.

The return value is formated to a string using %S, so a string will be
returned as an escaped embedded string and literal forms will be
compatible with `read' where possible.  Some forms have no printed
representation that can be read and will be represented with
#<hash-notation> instead.

You can use this to quickly change a user setting, check a variable, or
demonstrate something to the user.")

                                         (gptel-make-tool
                                          :function #'pmx--gptel-symbolp
                                          :name "symbol_exists"
                                          :include t
                                          :category "introspection"
                                          :args '(( :name "symbol"
                                                    :type string
                                                    :description "A symbol that will be in `obarray' if they \
actually exist"))
                                          :description "Check if SYMBOL exists in `obarray'. \
Returns the name of a symbol if that symbol has been interned or \"nil\"
if not.  Uses `intern-soft' to perform the check.  This tool is
extremely cheap to call.")

                                         (gptel-make-tool
                                          :function #'pmx--gptel-load-paths
                                          :name "load_paths"
                                          :include t
                                          :category "introspection"
                                          :args nil
                                          :description "Return the users load paths.
This can reveal information about what packages the user has available.
You can also learn about what kind of package management they are using
and which packages are likely shadowed by their Elisp dependency
manager.  The location of default packages can tell you about the user's
Emacs installation.")

                                         (gptel-make-tool
                                          :function #'pmx--gptel-features
                                          :name "features"
                                          :include t
                                          :category "introspection"
                                          :args nil
                                          :description "Return the list of loaded features.
This tool can be used to see what packages are already loaded in the
running Emacs.  Use this to understand the user's typical set of
packages and typical usage patterns.  Especially if the solution depends
on the user's choice of packages, you will want to look at the features
and load paths.")

                                         (gptel-make-tool
                                          :function #'pmx--gptel-manual-names
                                          :name "manual_names"
                                          :include t
                                          :category "introspection"
                                          :args nil
                                          :description "Return a list of available manual names.
Call this tool in order to determine if a particular manual is
available.  This can also help determine which packages are available on
the user's Emacs.  This tool is a good starting point for general
questions about Emacs, Elisp, and common built-in packages.

Manuals are usually named the same as the symbol of the package prefix
that they document.  The Common Lisp manual is called \"cl\".  The Emacs
Lisp manual is called \"elisp\".

You will usually follow this call with a subsequent call to
`manual_nodes' in order to see the sections in the manual, which are
somewhat like a summary.  This call is extremely cheap and should be
used liberally.")

                                         (gptel-make-tool
                                          :function #'pmx--gptel-manual-list-nodes
                                          :name "manual_nodes"
                                          :include t
                                          :category "introspection"
                                          :args '(( :name "manual"
                                                    :type string
                                                    :description "The name of the manual.
Examples include \"cl\", \"elisp\", or \"transient\"."))
                                          :description "Retrieve a listing of topic nodes within MANUAL.
Return value is a list of all nodes in MANUAL.  The list of topic nodes
provides a good summary of MANUAL.

MANUAL is one of the results returned from `manual_names'.  If you are
sure a manual exists, you may skip first calling `manual_names'.  When
you believe MANUAL exists, this tool is very useful to find places to
broaden your search.

You will usually follow this call with a subsequent call to
`manual_node_contents' to view the actual full contents of a node in the
manual.  This call is extremely cheap and should be used liberally.

In the Elisp manual, you can find more answers about code and
implementations that a programmer can used to deeply customize.  The
Emacs manual contains descriptions about built-in features and behavior
that can be used to understand the context for what is being programmed
in the Elisp manual.")

                                         (gptel-make-tool
                                          :function #'pmx--gptel-manual-node-contents
                                          :name "manual_node_contents"
                                          :include t
                                          :category "introspection"
                                          :args '(( :name "manual_name"
                                                    :type string
                                                    :description "The name of MANUAL.
Examples manuals include \"cl\", \"elisp\", or \"transient\".")
                                                  ( :name "node"
                                                          :type string
                                                          :description "The name of the NODE in a MANUAL.
Example nodes from the elisp manual include \"Records\" or \"Sequences
Arrays \ Vectors\"."))
                                          :description "Retrieve the contents of NODE in MANUAL.
The return value is the full contents of NODE in MANUAL.  Contents
include high-level grouping of related functions and variables.  Hidden
behavior is described.  This tool is awesome!  You should try to call it
all the time.

Pay attention to the entries in the Menu.  You can do recursive look-ups
of more specific manual sections.  Example menu:

* Menu:

* Good Node::
* A Node::

You can recursively look up \"Good Node\" and other relevant menu nodes
in this same MANUAL.  Sometimes there are links, such as, \"*Note
Narrowing::.\".  \"Narrowing\" is a node in this example.  Use this tool
recursively.

If both Elisp and Emacs manuals are available, open both but prefer Elisp manual
style language anc content.")

                                         (gptel-make-tool
                                          :function #'pmx--gptel-featurep
                                          :name "features"
                                          :include t
                                          :category "introspection"
                                          :args '(( :name "feature"
                                                    :type string
                                                    :description "FEATURE to look for."))
                                          :description "Check if FEATURE is loaded or available.
Returns non-nil if FEATURE is loaded or available for loading.  Not all
users have all features loaded.  Before recommending the user to try a
particular solution, you might check if the necessary features are
loaded.  If you are using all built-in Emacs libraries, you don't need
to check.  Use this mainly to check for 3rd party packages that the user
would obtain from MELPA and Non-GNU ELPA etc.")

                                         (gptel-make-tool
                                          :function #'pmx--gptel-library-source
                                          :name "library_source"
                                          :include t
                                          :category "introspection"
                                          :args '(( :name "library"
                                                    :type string
                                                    :description "LIBRARY to look for."))
                                          :description "Read the source code for LIBRARY.
LIBRARY can either be a C or Elisp source code library.  Examples would
include \"transient\" or \"fns.c\".  When looking for C libraries, they
must contain the .c suffix.

This tool is a bit expensive, and you can usually find what you want by
looking up symbols in the package first by calling
`function_completions' and `variable_completions' to get a high-level
summary of what definitions might be contained in a library.

Watch for for sub-packages.  Some multi-file packages will have symbols
that are defined in a sub-package.  If you see a common prefix in the
function or variable completions and those symbols are not in the
top-level package, there are likely sub-packages and you should
recursively look them up.")

                                         (gptel-make-tool
                                          :name "symbol_manual_section"
                                          :include t
                                          :function #'pmx--gptel-symbol-in-manual
                                          :category "introspection"
                                          :args '(( :name "symbol"
                                                    :type string
                                                    :description "Name of a SYMBOL, such as \
\"find-file-noselect\"."))
                                          :description "Returns contents of manual node for SYMBOL.
SYMBOL can be a function, macro, defcustom, or defvar.  If symbol is not
known to be in a manual, this functon will return nil.

The returned manual contents are similar to the `manual_node_contents'
tool.  You sould recursively inspect any links or menu entries that look
relevant.  Check the node list for the manual if a link or menu entry
returns nil.

If you can't find anything, you should try looking up its source or
docstring next and finally try to complete the prefix of the symbol .")

                                         (gptel-make-tool
                                          :name "function_source"
                                          :include t
                                          :function #'pmx--gptel-function-source
                                          :category "introspection"
                                          :args '(( :name "function"
                                                    :type string
                                                    :description "Name of a FUNCTION, such as \
\"find-file-noselect\"."))
                                          :description "Returns the source code for FUNCTION.
Return the source code for FUNCTION.  FUNCTION can be a function or
macro.  The signature and docstring can supply extremely valuable
information about how to call a function correctly and what behaviors
are controlled by its arguments.  You can understand the side-effects
and what variables a function reacts to by reading its body.

You can use the source code for functions to recursively look up other
functions & variables and make inferences about how implementations work
in order to connect the behaviors and implementation details that the
user will need.

Because the docstring is embedded in the source, you should prefer this
tool over just retrieving the documentation. If the result seems
incomplete, you can try returning the docstring using
`function_documentation' or the entire source for a library feature by
using `library_source'.  This tool is cheap.  Use it liberally.")

                                         (gptel-make-tool
                                          :name "variable_source"
                                          :function #'pmx--gptel-variable-source
                                          :category "introspection"
                                          :include t
                                          :args '(( :name "variable"
                                                    :type string
                                                    :description "Name of a VARIABLE, such as \
\"last-kbd-macro\"."))
                                          :description "Returns the source code for VARIABLE.
Return value is the source code for VARIABLE.  VARIABLE can be a defvar
or defcustom.  The returned source code can be extremely insightful
because nothing is more accurate than looking at the code and the source
code contains the docstring too.

You can use source code for variables to see the forms used in setting
their defaults and make inferences about what forms will be interpreted
correctly.  If the result seems incomplete, you can try returning the
docstring using `variable_documentation' or the entire source for a
library feature by using `library_source'.  This tool is cheap and fast.
Call it liberally.")

                                         (gptel-make-tool
                                          :name "variable_value"
                                          :function #'pmx--gptel-variable-global-value
                                          :category "introspection"
                                          :confirm t
                                          :include t
                                          :args '(( :name "variable"
                                                    :type string
                                                    :description "Name of a VARIABLE, such as \
\"last-kbd-macro\"."))
                                          :description "Returns the global value for VARIABLE.
Return value is the global (not buffer-local) value for VARIABLE.
VARIABLE can be a defvar or defcustom.  Use this when behavior depends
on the state of a variable or you want to infer if a package has indeed
configured a variable.  By observing expected side-effects, you can
build a coherent view of the interaction between functions and settings.

Use of this tool could leak private data, so don't call it for any
VARIABLE that contains initialized authentication data.

If the result is confusing, you can try returning the docstring using
`variable_documentation' to gain insights into the structure of values
contained.")

                                         (gptel-make-tool
                                          :name "function_documentation"
                                          :function #'pmx--gptel-function-documentation
                                          :category "introspection"
                                          :include t
                                          :args '(( :name "function"
                                                    :type string
                                                    :description "Name of a FUNCTION, such as \"mapcar\"."))
                                          :description "Returns the docstring for FUNCTION.
Return value is a docstring for FUNCTION.  FUNCTION can be a function or
macro.  Can be used to infer the purpose or correct forms for arguments
and behavior changes related to those arguments.  This is more reliable
than `function_source', so if `function_source' seems off, try this.
This tool is very cheap and very fast.  Call it very liberally.")

                                         (gptel-make-tool
                                          :name "variable_documentation"
                                          :function #'pmx--gptel-variable-documentation
                                          :category "introspection"
                                          :include t
                                          :args '(( :name "variable"
                                                    :type string
                                                    :description "Name of a VARIABLE, such as \
\"cursor-type\"."))
                                          :description "Returns the docstring VARIABLE.
Return value is a docstring for VARIABLE.  VARIABLE can be a defcustom
or defvar.  Can be used to infer the correct forms for setting a
variable, such as when configuring packages in use-package expressions
or leading the user through diagnosing something.  This tool is very
cheap and very fast.  Call it very liberally.")

                                         ;; TODO this tool can complete out of order using orderless
                                         (gptel-make-tool
                                          :name "function_completions"
                                          :function #'pmx--gptel-function-completions
                                          :category "introspection"
                                          :include t
                                          :args '(( :name "function_prefix"
                                                    :type string
                                                    :description "FUNCTION_PREFIX of functions you are searching for."))
                                          :description "Returns a list of functions beginning with FUNCTION_PREFIX.
Use this to prepare for subsequent calls to `function_source' or
`function_documentation' to look up the source code or docstrings of
multiple functions.  You can also use this tool to verify which
functions and macros can be called.  If you want to search for all
functions defined in foo and its sub-packages, you this tool is a very
good starting point.  This tool is very cheap and very fast.  Call it
very liberally.")

                                         ;;
                                         (gptel-make-tool
                                          :name "command_completions"
                                          :function #'pmx--gptel-command-completions
                                          :category "introspection"
                                          :include t
                                          :args '(( :name "command_prefix"
                                                    :type string
                                                    :description "COMMAND_PREFIX of commands you are searching for."))
                                          :description "Returns a list of commands beginning with COMMAND_PREFIX.
This tool is very similar to `function_completions' but will only return
commands that can be called interactively.  This can tell you about the
entry points where a user begins interacting with a package.  Because
commands are functions, you will follow up this tool with calls to
`function_source' and `function_documentation'.  This tool is very cheap
and very fast.  Call it very liberally.")

                                         ;; TODO this tool can complete out of order using orderless
                                         (gptel-make-tool
                                          :name "variable_completions"
                                          :function #'pmx--gptel-variable-completions
                                          :category "introspection"
                                          :include t
                                          :args '(( :name "variable_prefix"
                                                    :type string
                                                    :description "VARIABLE_PREFIX of variables you are searching for."))
                                          :description "Returns a list of variables beginning with VARIABLE_PREFIX.
The variables returned include defvars and custom variables.  Defvars
tell you what states a package relies on for its implementation.
Defcustom tells you what configuration options the user should know
about when configuring a package, such as if they are working on
use-package expressions.

Use this to prepare for subsequent calls to `variable_source' or
`variable_documentation' to look up the source code or docstrings of
multiple variables.  If you want to search for all variables defined
under a prefix, you this tool is a very good starting point.  This tool
is very cheap and very fast.  Call it very liberally.")

                                         (gptel-make-tool
                                          :name "simulate_error"
                                          :function #'pmx--gptel-simulate-error
                                          :category "testing"
                                          :args nil
                                          :description "A tool that can simulate an error.
This tool always returns an error.  It is useful for testing error
behavior.  When the user asks you to use this tool, you should
absolutely use it.")

                                         (gptel-make-tool
                                          :name "coerce_nil"
                                          :function #'pmx--gptel-coerce-nil
                                          :category "testing"
                                          :args nil
                                          :description "A tool that returns nil.
Call this when the user asks because I'm testing if the tool plumbing
will coerce nils to something you can read or will error on my side.")

                                         (gptel-make-tool
                                          :name "all_arg_types"
                                          :function #'pmx--gptel-all-arg-types
                                          :category "testing"
                                          :include t
                                          :args '((:name "an_object" :type object :description "A basic object"
                                                   :properties (:foo (:type integer :description "Use 42"))
                                                   :required ["foo"])
                                                  (:name "string" :type string :description "A string")
                                                  (:name "array" :type array :description "An array"
                                                   :items (:type number))
                                                  (:name "null" :type null :description "A null")
                                                  (:name "true" :type boolean :description "Must be true")
                                                  (:name "false" :type boolean :description "Must be false")
                                                  (:name "enum" :type string :description "A choice"
                                                   :enum ["bar" "baz" "boo"]))
                                          :description "A function the user wants to test out.")

                                         (gptel-make-tool
                                          :name "async_tool"
                                          :function #'pmx--gptel-async-tool
                                          :category "testing"
                                          :include t
                                          :async t
                                          :args
                                          '((:name "later-val" :type string :description "Just whenever."))
                                          :description "A tool that takes a while.
If the user says call it, always call it."))

                                        ))
(provide 'agent)
