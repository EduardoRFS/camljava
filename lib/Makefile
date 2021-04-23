include ../Makefile.config

OCAMLLIB=""
CAMLJAVALIB=$(OCAMLLIB)/camljava

jni.ml: jni.mlp ../Makefile.config
	rm -f jni.ml
	sed -e 's|%PATH%|'$(CAMLJAVALIB)/camljava.jar'|' \
                jni.mlp > jni.ml
	chmod -w jni.ml

clean::
	rm -f jni.ml

c_flags.sexp:
	echo "($(CFLAGS) $(JNIINCLUDES))" > c_flags.sexp

c_library_flags.sexp:
	echo '(-ccopt "$(JNILIBOPTS)" -cclib "$(JNILIBS)")' > c_library_flags.sexp