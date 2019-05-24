hs_target = blog
hs_opts = -O2
site_build_dir = _site.production
config_file = config.production
other_site_options = ""
git_options = ""

generate-site:
	cabal new-build $(hs_target) $(hs_opts)
	cp -f `find dist-newstyle -name $(hs_target) -perm +u+x -type f | head -n 1` ./generate-site

site: generate-site
	./generate-site --builddir=$(site_build_dir) --config-file=$(config_file) --no-color $(other_site_options)

commit:
	./scripts/commit.fish $(site_build_dir) $(git_options)

new-post:
	./scripts/new-post.fish -t "$(title)" -s $(slug) -d "$(date)"

.PHONY: new-post commit
