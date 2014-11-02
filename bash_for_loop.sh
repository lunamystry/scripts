for filename in *
do
    if [ -d $filename ]; then
        cd $filename
        chmod 664 *
        cd ..
    fi
done
