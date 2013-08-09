OBJS = SyntaxTree.o main.o

main: $(OBJS)
	c++ -O3 -g -o $@ $(OBJS)

.cpp.o:
	c++ -O3 -g -c $<

.PHONY: clean

clean:
	rm -f *.o *~ main