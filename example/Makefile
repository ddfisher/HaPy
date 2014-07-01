UNAME := $(shell uname)
APT_EXISTS := $(shell command -v apt-get > /dev/null && echo "Y" || echo "N")
all:
	@echo === Installing Dependencies ===
ifeq ($(UNAME),Linux)
ifeq ($(APT_EXISTS),Y)
	@echo '>>> sudo apt-get install ghc-dynamic'
	@sudo apt-get install ghc-dynamic
else
	@echo 'UNKNOWN LINUX DISTRO: please make sure you have dynamic Haskell base libraries installed.'
endif
	@echo
endif
	@echo '>>> sudo pip install hapy-ffi'
	@sudo pip install hapy-ffi
	@echo
	@$(MAKE) -C haskell all
	@echo

clean:
	@$(MAKE) -C haskell clean
	@echo
	@$(MAKE) -C python clean

run: all
	@$(MAKE) -C python run
