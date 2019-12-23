#!/bin/bash

FROM="Jul 1 2018"

declare -a PEOPLE=("Alexei Starovoitov"
		   "Andrey Ignatov"
		   "Dennis Zhou"
		   "Calvin Owens"
		   "Chris Mason"
		   "Dan Melnic"
		   "Dave Jones"
		   "Jens Axboe"
		   "Jes Sorensen"
		   "Johannes Weiner"
		   "Josef Bacik"
		   "Kyle McMartin"
		   "Lawrence Brakmo"
		   "Martin KaFai Lau"
		   "Omar Sandoval"
		   "Phillip Duncan"
		   "Rik van Riel"
		   "Roman Gushchin"
		   "Shaohua Li"
		   "Song Liu"
		   "Tejun Heo"
		   "Yannick Brosseau"
		   "Yonghong Song"
		   "Konstantin Khlebnikov"
		   "Mike Rapoport"
		   "Andrei Vagin")

for i in "${PEOPLE[@]}"; do
    commits=`git log next/master --oneline --no-merges --author="${i}" --since="${FROM}"`
    N=`git log next/master --oneline --no-merges --author="${i}" --since="${FROM}" | wc -l`
    echo -e "${N}\t${i}"
    echo -e "${commits}"
done
