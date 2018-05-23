#! /usr/bin/env fish

function make_slug
    set -l sans_nonsense (string replace --all --regex \\W+ - $argv[1])
    set -l sans_nonsense (string lower $sans_nonsense)
    string escape --style=url $sans_nonsense
end

set title $argv[1]
set slug (make_slug $title)
set filename posts/$slug.md
set date (date "+%e %b %Y")
set dummy_synopsis "In which I write a blog post."

echo "---" > $filename
echo "title: $title" >> $filename
echo "date: $date" >> $filename
echo "synopsis: $dummy_synopsis" >> $filename
echo "---" >> $filename
echo "" >> $filename
echo "Good evening, internet!" >> $filename

vim $filename
