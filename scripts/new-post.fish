#! /usr/bin/env fish

argparse --name new-post t/title= 's/slug=?' d/date= -- $argv
or exit

set title $_flag_title

if set -q _flag_slug
    and string match --regex --quiet '\w' $_flag_slug
    set slug $_flag_slug
else
    set -l sans_nonsense (string replace --all --regex '\W+' - $title)
    set -l sans_nonsense (string lower $sans_nonsense)
    set slug (string escape --style=url $sans_nonsense)
end

set date_format "%e %B %Y"
if set -q _flag_date
    switch $_flag_date
        case today
            set date (date +"$date_format")
        case yesterday
            set date (date -v -1d +"$date_format")
        case tomorrow
            set date (date -v +1d +"$date_format")
        case '+*'
            set date (date -v $_flag_date +"$date_format")
        case '-*'
            set date (date -v $_flag_date +"$date_format")
        case ''
            set date (date +"$date_format")
        case '*'
            set date $_flag_date
    end
else
    set date (date +"$date_format")
end

set filename posts/$slug.md
set dummy_synopsis "In which I write a blog post."

echo "---" > $filename
echo "title: $title" >> $filename
echo "date: $date" >> $filename
echo "synopsis: $dummy_synopsis" >> $filename
echo "---" >> $filename
echo "" >> $filename
echo "Good evening, internet!" >> $filename
