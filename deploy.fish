#! /usr/bin/env fish

pushd $argv[1]
set git_status (git status --porcelain)
if count $git_status
    git add .
    git commit -a
    git push
else
    echo Nothing new to deploy!
end
popd
