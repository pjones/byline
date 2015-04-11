################################################################################
.PHONEY: all install restart

################################################################################
# Set up the default target.
all::

################################################################################
# Ask `git' to update the submodule and make haskell.mk available.
util/haskell.mk:
	git submodule update --init

################################################################################
# From util/haskell.mk (git submodule update --init)
CABAL_FLAGS = --enable-tests -fmaintainer
include util/haskell.mk
