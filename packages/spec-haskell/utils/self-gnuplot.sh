# gnuplot a file whose first line is a gnuplot command, followed by data
TMP=`mktemp`
cat - > $TMP
E=`head -n1 $TMP | sed "s:__GNUPLOT_FILE__:$TMP:g"`
echo $E
gnuplot -p -e "$E"
rm -f $TMP
