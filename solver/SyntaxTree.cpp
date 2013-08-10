#include <string>
#include <sstream>
#include <iostream>
#include "SyntaxTree.h"

using namespace std;

unsigned long long var[3];

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

SyntaxTree::SyntaxTree(Operation op, int varNum, const SyntaxTree* child1, const SyntaxTree* child2, const SyntaxTree* child3) {
    this->op = op;
    this->varNum = varNum;
    const SyntaxTree* childs[3] = {child1, child2, child3};
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

string SyntaxTree::toString() const {
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
            this->child[1]->toString() + " (lambda (" + this->varName(1) + " " +
            this->varName(2) + ") " + this->child[2]->toString() + "))";
    }
}
    
string SyntaxTree::opName() const {
    return operationName[this->op];
}

string SyntaxTree::varName(int n) const {
    stringstream ss;
    ss << "v";
    ss << n;
    return ss.str();
}

unsigned long long SyntaxTree::eval(unsigned long long x) const {
    var[0] = x;
    this->evalSub();
}

unsigned long long SyntaxTree::evalSub() const {
    switch (this->op) {
    case ZERO:
        return 0;
    case ONE:
        return 1;
    case VAR:
        return var[this->varNum];
    case NOT:
        return ~(this->child[0]->evalSub());
    case SHL1:
        return this->child[0]->evalSub() << 1LL;
    case SHR1:
        return this->child[0]->evalSub() >> 1LL;
    case SHR4:
        return this->child[0]->evalSub() >> 4LL;
    case SHR16:
        return this->child[0]->evalSub() >> 16LL;
    case AND:
        return this->child[0]->evalSub() & this->child[1]->evalSub();
    case OR:
        return this->child[0]->evalSub() | this->child[1]->evalSub();
    case XOR:
        return this->child[0]->evalSub() ^ this->child[1]->evalSub();
    case PLUS:
        return this->child[0]->evalSub() + this->child[1]->evalSub();
    case IF0:
        return (!this->child[0]->evalSub() ? this->child[1]->evalSub() : this->child[2]->evalSub());
    case FOLD:
        unsigned long long x = this->child[0]->evalSub();
        var[2] = this->child[1]->evalSub();
        for (int i = 0; i < 8; i++) {
            var[1] = (x >> i * 8) & 0xFF;
            var[2] = this->child[2]->evalSub();
        }
        return var[2];
    }
    return 0;
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

bool SyntaxTree::operator<(const SyntaxTree& obj) const {
    if (this->op < obj.op)return true;
    if (this->op > obj.op)return false;
    if (this->op == VAR)return this->varNum < obj.varNum;

    for (int i = 0; i < operationChildNum[op]; i++) {
        if (*(this->child[i]) < *(obj.child[i]))return true;
        if (*(this->child[i]) > *(obj.child[i]))return false;
    }
    return false;
}

bool SyntaxTree::operator<=(const SyntaxTree& obj) const {
    return !(*this > obj);
}

bool SyntaxTree::operator>(const SyntaxTree& obj) const {
    if (this->op > obj.op)return true;
    if (this->op < obj.op)return false;
    if (this->op == VAR)return this->varNum > obj.varNum;

    for (int i = 0; i < operationChildNum[op]; i++) {
        if (*(this->child[i]) > *(obj.child[i]))return true;
        if (*(this->child[i]) < *(obj.child[i]))return false;
    }
    return false;
}

bool SyntaxTree::operator>=(const SyntaxTree& obj) const {
    return !(*this < obj);
}

bool SyntaxTree::operator==(const SyntaxTree& obj) const {
    if (this->op < obj.op)return false;
    if (this->op > obj.op)return false;
    if (this->op == VAR)return this->varNum == obj.varNum;

    for (int i = 0; i < operationChildNum[op]; i++) {
        if (*(this->child[i]) < *(obj.child[i]))return false;
        if (*(this->child[i]) > *(obj.child[i]))return false;
    }
    return true;
    
}
