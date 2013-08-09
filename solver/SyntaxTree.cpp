#include <string>
#include <sstream>
#include <iostream>
#include "SyntaxTree.h"

using namespace std;

std::string operationName[] = {
    "0",
    "1",
    "v",
    "not",
    "shl1",
    "shr1",
    "shr4",
    "shr16",
    "and",
    "or",
    "xor",
    "plus",
    "if0",
    "fold"
};

int operationChildNum[] = {
    0,0,0,
    1,1,1,1,1,
    2,2,2,2,
    3,3
};

SyntaxTree::SyntaxTree() {
}

SyntaxTree::SyntaxTree(Operation op, int varNum, SyntaxTree** childs) {
    this->op = op;
    this->varNum = varNum;
    for (int i = 0; i < operationChildNum[op]; i++) {
        SyntaxTree* c = new SyntaxTree();
        *c = *(childs[i]);
        this->child[i] = c;
    }
}

SyntaxTree::SyntaxTree(const SyntaxTree& obj) {
    this->op = obj.op;
    this->varNum = obj.varNum;
    for (int i = 0; i < operationChildNum[op]; i++) {
        SyntaxTree* c = new SyntaxTree();
        *c = *(obj.child[i]);
        this->child[i] = c;
    }
}

SyntaxTree::~SyntaxTree() {
    for (int i = 0; i < operationChildNum[op]; i++) {
        delete this->child[i];
    }
}

SyntaxTree& SyntaxTree::operator=(const SyntaxTree& obj){
    this->op = obj.op;
    this->varNum = obj.varNum;
    for (int i = 0; i < operationChildNum[op]; i++) {
        SyntaxTree* c = new SyntaxTree();
        *c = *(obj.child[i]);
        this->child[i] = c;
    }
}

string SyntaxTree::toString() {
    switch (this->op) {
    case ZERO:
    case ONE:
        return this->opName();
    case VAR:
        return this->varName(varNum);
    case NOT:
    case SHL1:
    case SHR1:
    case SHR4:
    case SHR16:
        return "(" + this->opName() + " " + this->child[0]->toString() + ")"; 
    case AND:
    case OR:
    case XOR:
    case PLUS:
        return "(" + this->opName() + " " + this->child[0]->toString() + " " + this->child[1]->toString() + ")";
    case IF0:
        return "(" + this->opName() + " " + this->child[0]->toString() + " " +
            this->child[1]->toString() + " " + this->child[2]->toString() + ")";
    case FOLD:
        return "(" + this->opName() + " " + this->child[0]->toString() + " " +
            this->child[1]->toString() + " (lambda (" + this->varName(varNum) + " " +
            this->varName(varNum + 1) + ") " + this->child[2]->toString() + "))";
    }
}
    
string SyntaxTree::opName() {
    return operationName[this->op];
}

string SyntaxTree::varName(int n) {
    stringstream ss;
    ss << "v";
    ss << n;
    return ss.str();
}

