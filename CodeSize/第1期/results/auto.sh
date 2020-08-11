exclude="clang-cortex-m0.cmake clang-cortex-m4.cmake gcc-cortex-m0.cmake gcc-cortex-m4.cmake"
dir=$(ls ../toolchain-files/)
for file in $dir
do 
    sname=$(basename $file)
    if ! [[ "$exclude" =~ "$sname" ]]
    then
          echo $sname
          ../csibe.py -j32 --build-dir=./temp/ --toolchain ${sname/".cmake"/""} CSiBE-v2.1.1
    fi 
done

./getFiles.py
./plotResults.py "os_o3os" 1 2 3 4 5 6 7 8
./plotResults.py "x32_x64" 1 3 2 4 5 7 6 8
./plotResults.py "clang_gcc" 1 3 2 4 5 7 6 8