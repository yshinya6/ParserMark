for x in `ls ../../test/*.cello`; do
  java CelloParser $x
done
