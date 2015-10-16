SWAP		= /mnt/swap
PANDOC_VERSION 	= 1.15.0.6
PANDOC		= scarlet/pandoc-$(PANDOC_VERSION)
PANDOC_PATCH	= $(PANDOC)-text-shared.patch
HALCYON		= /app/halcyon/halcyon
HALCYON_OPTS 	= HALCYON_GHC_VERSION=7.10.1
GIT		= /usr/bin/git
SCARLET_BRANCH	= parametrize-in-makefile

scarlet/dist: scarlet scarlet/pandoc-$(PANDOC_VERSION) $(HALCYON) $(SWAP)
	$(HALCYON_OPTS) $(HALCYON) install scarlet

$(SWAP):
	sudo dd if=/dev/zero of=$(SWAP) bs=1M count=2048
	sudo chmod 600 $(SWAP)
	sudo mkswap $(SWAP)
	sudo swapon $(SWAP)

scarlet: $(GIT)
	$(GIT) clone https://github.com/rhwlo/Scarlet scarlet
	cd scarlet && git checkout $(SCARLET_BRANCH)

$(PANDOC): scarlet $(PANDOC_PATCH)
	cd scarlet && curl "http://hackage.haskell.org/package/pandoc-$(PANDOC_VERSION)/pandoc-$(PANDOC_VERSION).tar.gz" | tar xvz
	cd scarlet && patch -p1 < $(PANDOC_PATCH)

$(GIT):
	sudo yum install git

$(HALCYON):
	curl -sLO https://github.com/mietek/halcyon/raw/master/setup.sh
	bash setup.sh
