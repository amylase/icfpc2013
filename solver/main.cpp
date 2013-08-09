#include <iostream>
#include "SyntaxTree.h"

using namespace std;

int main() {
    SyntaxTree* zero = new SyntaxTree(ZERO);
    SyntaxTree* one = new SyntaxTree(ONE);
    SyntaxTree* x = new SyntaxTree(VAR, 0);
    SyntaxTree* y = new SyntaxTree(VAR, 1);
    SyntaxTree* z = new SyntaxTree(VAR, 2);
    SyntaxTree* oryz = new SyntaxTree(PLUS, 3, y, z);
    SyntaxTree* fold = new SyntaxTree(FOLD, 1, x, zero, oryz);

    cout << fold->toString() << endl;
    cout << (*fold == *fold) << endl;
    cout << (*fold < *fold) << endl;
    cout << (*fold <= *fold) << endl;
    cout << (*x < *y) << endl;
    cout << (*oryz < *fold) << endl;
    
    cout << fold->eval(72623859790382856ULL) << endl;
    return 0;
}
