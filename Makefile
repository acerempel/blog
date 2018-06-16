hs_target = blog
hs_opts = -O2
site_build_dir = _site.production
site_opts = --builddir=$(site_build_dir) --config-file=config.production --no-color

build:
	cabal new-run $(hs_target) $(hs_opts) -- $(site_opts)

commit-built-site:
	./scripts/commit.fish $(site_build_dir)

deploy-github: commit-built-site
	./scripts/deploy-github.fish $(site_build_dir)

deploy-neocities: commit-build-site
	neocities push $(site_build_dir)

new-post:
	./scripts/new-post.fish -t "$(title)" -s $(slug) -d "$(date)"

.PHONY: deploy build new-post deploy-github deploy-neocities commit-build-site
