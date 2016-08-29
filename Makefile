SC = chez-scheme -q --optimize-level 2
SRC = mattie/parser.scm mattie/interpreter.scm mattie/parser/combinators.scm
OBJ = $(SRC:.scm=.so)

test:
	@./run-tests.scm

clean:
	@find -name "*.so" -delete

%.so: %.scm
	@echo '(compile-file "$<")' | $(SC)

mattie.so: $(OBJ)
	@cat $(OBJ) > $@

.PHONY: test clean
