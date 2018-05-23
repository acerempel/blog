hs_target = blog
hs_opts = -O2
site_build_dir = _site.production
site_opts = --builddir=$(site_build_dir) --config-file=config.production --no-color

build:
	cabal new-run $(hs_target) $(hs_opts) -- $(site_opts)

deploy: build
	./deploy.fish $(site_build_dir)

.PHONY: deploy build
