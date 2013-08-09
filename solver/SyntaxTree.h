#pragma once
#include <string>

enum Operation {
    ZERO,
    ONE,
    VAR,
    NOT,
    SHL1,
    SHR1,
    SHR4,
    SHR16,
    AND,
    OR,
    XOR,
    PLUS,
    IF0,
    FOLD
};

class SyntaxTree {
public:
    Operation op;
    int varNum;
    SyntaxTree* child[3];

    SyntaxTree();
    SyntaxTree(Operation, int varNum = 0, SyntaxTree** childs = NULL);
    SyntaxTree(const SyntaxTree&);
    ~SyntaxTree();
    SyntaxTree& operator=(const SyntaxTree&);
    std::string toString();
    std::string opName();
    std::string varName(int v);
    
};
