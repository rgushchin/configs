#!/bin/bash

FROM="Jan 1 2020"

declare -a PEOPLE=("Alexei Starovoitov"
		   "Andrey Ignatov"
		   "Dennis Zhou"
		   "Chris Mason"
		   "Jens Axboe"
		   "Johannes Weiner"
		   "Josef Bacik"
		   "Martin KaFai Lau"
		   "Omar Sandoval"
		   "Rik van Riel"
		   "Roman Gushchin"
		   "Song Liu"
		   "Tejun Heo"
		   "Yannick Brosseau"
		   "Yonghong Song"
		   "Konstantin Khlebnikov"
		   "Stanislav Fomichev")

for i in "${PEOPLE[@]}"; do
    commits=`git log next/master --oneline --no-merges --author="${i}" --since="${FROM}"`
    N=`git log next/master --oneline --no-merges --author="${i}" --since="${FROM}" | wc -l`
    echo -e "${N}\t${i}"
    echo -e "${commits}"
done
