mochosa.exe: mochosa.lisp compose-message-dialog.lisp notification-settings-dialog.lisp
	sbcl --load mochosa.lisp --eval "(sb-ext:save-lisp-and-die \"mochosa.exe\" :toplevel #'mochosa::main :executable t)"
