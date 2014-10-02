rm aabb EXT/aabb
#for ii in `find . -name "*" | grep -v "ksh" |grep -v "EXT"`
#for ii in `find . -name "*" | grep -v "ksh"|grep -v vegs `
for ii in `find . -name "*.in" | grep -v "ksh" `
do
	echo $ii
	#grep -e ": 47" -e ": 46" $ii
	sed 's/: 53$/: 51/g;' $ii >$ii.tmp
	mv -f $ii.tmp $ii
done
