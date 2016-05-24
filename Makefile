BIN=xmonad-$(shell uname -m)-$(shell uname | tr A-Z a-z)
$(BIN): xmonad.hs
	ghc --make -O -dynamic -Wall -fno-warn-tabs $< -i -ilib -o $@
clean:
	rm -f $(BIN) *.hi *.o lib/*.hi lib/*.o
.PHONY: $(BIN) clean
