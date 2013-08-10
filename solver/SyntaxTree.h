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
    SyntaxTree(Operation, int varNum = 0, const SyntaxTree* child1 = NULL, const SyntaxTree* child2 = NULL, const SyntaxTree* child3 = NULL);
    SyntaxTree(const SyntaxTree&);
    ~SyntaxTree();
    std::string toString() const;
    std::string opName() const;
    std::string varName(int v) const;
    unsigned long long eval(unsigned long long x) const;
    unsigned long long evalSub() const;

    SyntaxTree& operator=(const SyntaxTree&);
    bool operator<(const SyntaxTree&) const;
    bool operator<=(const SyntaxTree&) const;
    bool operator>(const SyntaxTree&) const;
    bool operator>=(const SyntaxTree&) const;
    bool operator==(const SyntaxTree&) const;
};
