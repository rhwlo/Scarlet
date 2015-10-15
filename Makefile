GIT_ROOT		= $(CURDIR)
CABAL_SANDBOX_ROOT	= $(GIT_ROOT)/.cabal-sandbox
CABAL_SANDBOX_LIBS  	= $(CABAL_SANDBOX_ROOT)/lib
CABAL_SANDBOX_BIN	= $(CABAL_SANDBOX_ROOT)/bin
CABAL_VERSION		= 1.22.6.0
GHC_VERSION		= 7.10.1
HALCYON_OPTIONS		= HALCYON_GHC_VERSION=$(GHC_VERSION)\
				HALCYON_CABAL_VERSION=$(CABAL_VERSION)\
				HALCYON_BASE=$(HALCYON_ROOT)
HALCYON_ROOT		= $(GIT_ROOT)/.halcyon
HALCYON_CABAL		= $(HALCYON_ROOT)/cabal/bin/cabal
CABAL			= $(CABAL_SANDBOX_BIN)/cabal
HALCYON			= $(HALCYON_OPTIONS) $(HALCYON_ROOT)/halcyon/bin/halcyon
HALCYON_SETUP_URL	= https://github.com/mietek/halcyon/raw/master/setup.sh
PANDOC			= pandoc-1.15.0.6
PANDOC_PATCH		= $(PANDOC)-text-shared.patch
PANDOC_PATCH_URL	= https://github.com/rhwlo/Scarlet/raw/$(PANDOC_PATCH)
SCARLET_PORT 		= 4040

dist: $(CABAL_SANDBOX_LIBS)
	$(CABAL) build

run: dist
	$(CABAL) run scarlet $(SCARLET_PORT)

clean: $(CABAL)
	$(CABAL) clean

$(CABAL_SANDBOX_LIBS): $(CABAL) $(PANDOC) $(HALCYON_ROOT)/alex $(HALCYON_ROOT)/happy
	$(CABAL) update
	$(CABAL) install --dependencies-only

$(PANDOC): $(CABAL) $(PANDOC_PATCH)
	$(CABAL) unpack $(PANDOC)
	patch -p2 < $(PANDOC_PATCH)
	$(CABAL) install $(PANDOC)

$(PANDOC_PATCH):
	curl -sL -o $(PANDOC_PATCH) $(PANDOC_PATCH_URL)

$(CABAL): $(CABAL_SANDBOX_ROOT) $(HALCYON_CABAL)
	$(HALCYON_CABAL) update
	$(HALCYON_CABAL) install cabal-install

$(CABAL_SANDBOX_ROOT): $(HALCYON_CABAL)
	$(HALCYON_CABAL) sandbox init

$(HALCYON_ROOT)/alex: $(HALCYON_ROOT)
	$(HALCYON) install alex

$(HALCYON_ROOT)/happy: $(HALCYON_ROOT)
	$(HALCYON) install happy

$(HALCYON_ROOT):
	curl -sL -o halcyon_setup.sh $(HALCYON_SETUP_URL)
	mkdir -p $(HALCYON_ROOT)
	sh halcyon_setup.sh
	$(HALCYON) install
