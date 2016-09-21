for x in {A..Z};do
      java Main `ls ../../../../bench/pmark/java/$x/*.java`
done
