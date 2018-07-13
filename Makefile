hs_target = blog
hs_opts = -O2
site_build_dir = _site.production
site_opts = --builddir=$(site_build_dir) --config-file=config.production --no-color
git_options = ""

generate-site:
	cabal new-build $(hs_target) $(hs_opts)
	cp -f `find dist-newstyle -name $(hs_target) -perm +u+x -type f | head -n 1` ./generate-site

site: generate-site $(site_build_dir)/index.js
	./generate-site $(site_opts)

$(site_build_dir)/index.js: index.js rollup.config.js yarn.lock
	rollup -c

yarn.lock: package.json
	yarn install

commit:
	./scripts/commit.fish $(site_build_dir) $(git_options)

new-post:
	./scripts/new-post.fish -t "$(title)" -s $(slug) -d "$(date)"

.PHONY: generate-site new-post commit
