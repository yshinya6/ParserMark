java -cp Mouse-1.6.1.jar: mouse.Generate -G resource/Epsilon.peg -P Epsilon 
#gsed -i -e "1i package resource;" Epsilon.java
sed -i -e "1i package resource;" Epsilon.java
mv Epsilon.java resource/
javac -cp Mouse-1.6.1.jar: resource/Epsilon.java
