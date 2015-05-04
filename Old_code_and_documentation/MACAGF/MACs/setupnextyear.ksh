rm *.tmp EXT/*.tmp
for ii in `find . -type f -name "*" | grep -v "ksh" | grep -v svn `
do
	echo $ii
	 's/47;/53;/g; s/2007;/2013;/g' $ii > $ii.tmp
	mv $ii.tmp $ii
done
