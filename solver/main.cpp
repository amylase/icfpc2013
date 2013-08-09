#include <iostream>
#include "SyntaxTree.h"

using namespace std;

int main() {
    SyntaxTree* zero = new SyntaxTree(ZERO);
    SyntaxTree* one = new SyntaxTree(ONE);
    SyntaxTree* v = new SyntaxTree(VAR, 0);
    SyntaxTree* child[] = {zero, v, one};
    SyntaxTree* test = new SyntaxTree(FOLD, 1, child);
    SyntaxTree* child2[] = {test, one};
    SyntaxTree* test2 = new SyntaxTree(AND, 1, child2);
    delete test;

    cout << test2->toString() << endl;
    return 0;
}
