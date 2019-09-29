#!/bin/csh
echo "You should be located in your own vista subdirectory"
echo "You will need to know the full path of the top level xvista directory"
echo -n "Enter the full path name (no trailing slash): "
set vistadir = $<
echo ""
mkdir user
cp $vistadir/source/user/* user
ln -s $vistadir user/vistadisk
ln -s $vistadir vistadisk
cp $vistadir/source/Makefile .
cp $vistadir/source/vista.F .
mkdir lickmongo
ln -s $vistadir/source/mongosrc lickmongo/source
