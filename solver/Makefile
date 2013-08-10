OBJS = SyntaxTree.o main.o

main: $(OBJS)
	c++ -O3 -std=c++0x -o $@ $(OBJS)

.cpp.o:
	c++ -O3 -std=c++0x -c $<

.PHONY: clean

clean:
	rm -f *.o *~ main