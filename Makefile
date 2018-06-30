hs_target = blog
hs_opts = -O2
site_build_dir = _site.production
site_opts = --builddir=$(site_build_dir) --config-file=config.production --no-color
git_options = ""

build: $(site_build_dir)/index.js
	cabal new-run $(hs_target) $(hs_opts) -- $(site_opts)
	./scripts/typeset.fish $(site_build_dir)

$(site_build_dir)/index.js: index.js
	cp -f index.js $(site_build_dir)/

commit:
	./scripts/commit.fish $(site_build_dir) $(git_options)

deploy-github: commit
	cd $(site_build_dir); git push

deploy-neocities: commit
	neocities push $(site_build_dir)

new-post:
	./scripts/new-post.fish -t "$(title)" -s $(slug) -d "$(date)"

.PHONY: build new-post deploy-github deploy-neocities commit
