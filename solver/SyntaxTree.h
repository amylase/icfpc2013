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
    SyntaxTree(Operation, int varNum = 0, SyntaxTree* child1 = NULL, SyntaxTree* child2 = NULL,SyntaxTree* child3 = NULL);
    SyntaxTree(const SyntaxTree&);
    ~SyntaxTree();
    std::string toString();
    std::string opName();
    std::string varName(int v);
    unsigned long long eval(unsigned long long x);
    unsigned long long evalSub();

    SyntaxTree& operator=(const SyntaxTree&);
    bool operator<(const SyntaxTree&);
    bool operator<=(const SyntaxTree&);
    bool operator>(const SyntaxTree&);
    bool operator>=(const SyntaxTree&);
    bool operator==(const SyntaxTree&);
};
