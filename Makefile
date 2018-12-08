MAKEFLAGS = -rR
CARGO = cargo

ifdef V
E=@#
Q=
else
E=@echo
Q=@
endif

.PHONY: test clean

test:
	$E $(CARGO) test
	$Q$(CARGO) test

clean:
	$E $(CARGO) clean
	$Q$(CARGO) clean
