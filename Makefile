BIN=xmonad-$(shell uname -m)-$(shell uname | tr A-Z a-z)
$(BIN): xmonad.hs
	ghc --make -Wall $< -i -ilib -o $@
.PHONY: $(BIN)
