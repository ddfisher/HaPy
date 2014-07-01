UNAME := $(shell uname)
SED_COMMAND := $(shell printf '/extra-libraries/ c \\\n  extra-libraries:     HSrts-ghc$(GHC_VERSION)')
all:
	@echo === Building Haskell ExampleModule ===
	@echo '>>> ./update_libghc_version.sh'
	@./update_libghc_version.sh
	@echo '>>> cabal sandbox init'
	@cabal sandbox init
	@echo
	@echo '>>> cabal install --enable-shared'
	@cabal install --enable-shared
	@echo
ifeq ($(UNAME),Linux)
	@echo '>>> cp dist/dist-sandbox-*/build/*.so ../python'
	@cp dist/dist-sandbox-*/build/*.so ../python
else ifeq ($(UNAME),Darwin)
	@echo '>>> cp dist/dist-sandbox-*/build/*.dylib ../python'
	@cp dist/dist-sandbox-*/build/*.dylib ../python
else
	@echo 'Unsupported OS!'
endif

clean:
	@echo === Cleaning Haskell Build ===
	rm -f cabal.sandbox.config
	rm -rf .cabal-sandbox
	rm -rf dist
