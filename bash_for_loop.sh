for filename in *
do
    if [ -d $filename ]; then
        cd $filename
        ls
        cd ..
    fi
done
