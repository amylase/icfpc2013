#include <iostream>
#include <vector>
#include <set>
#include <map>
#include "SyntaxTree.h"

using namespace std;

typedef set<SyntaxTree> exprset;

vector<Operation> op1s;
vector<Operation> op2s;
bool if0f, foldf;
exprset memo[30][2][2];
bool vis[30][2][2];

SyntaxTree zero(ZERO);
SyntaxTree one(ONE);
SyntaxTree x(VAR, 0);
SyntaxTree y(VAR, 1);
SyntaxTree z(VAR, 2);


bool same_prefix(const SyntaxTree& e1, const SyntaxTree& e2) {
    if (NOT <= e1.op && e1.op <= SHR16) return e1.op == e2.op;
    if (AND <= e1.op && e1.op <= PLUS) return e1.op == e2.op && (*e1.child[0]) == (*e2.child[0]);
    return false;
}

bool can_optimize_expr(const SyntaxTree& expr) {
    const SyntaxTree *c0 = expr.child[0], *c1 = expr.child[1], *c2 = expr.child[2];
    switch (expr.op) {
    case ZERO:
    case ONE:
    case VAR:
        return false;
    case NOT:
    case SHL1:
    case SHR1:
    case SHR4:
    case SHR16:
        if (can_optimize_expr(*c0)) return true;
        if (expr.op == NOT && c0->op == NOT) return true;
        return false;
    case AND:
    case OR:
    case XOR:
    case PLUS:
        if (can_optimize_expr(*c0)) return true;
        if (can_optimize_expr(*c1)) return true;
        if (*c0 > *c1) return true;
        if ((expr.op == AND || expr.op == OR || expr.op == XOR) && (c0->op == ZERO || c1->op == ZERO || (c0->op == ONE && c1->op == ONE))) return true;
        if (expr.op == PLUS && (c0->op == ZERO || c1->op == ZERO)) return true;
        return false;
    case IF0:
        if (can_optimize_expr(*c0)) return true;
        if (can_optimize_expr(*c1)) return true;
        if (can_optimize_expr(*c2)) return true;
        if (c0->op == ZERO || c0->op == ONE || *c1 == *c2) return true;
        if (same_prefix(*c1, *c2))return true;
        return false;
    case FOLD:
        if (can_optimize_expr(*c0)) return true;
        if (can_optimize_expr(*c1)) return true;
        if (can_optimize_expr(*c2)) return true;
        return false;
    }
    return false;
}

exprset* enumerate_expr(int n, bool fold, bool bound) {
    if (vis[n][fold][bound]) return &(memo[n][fold][bound]);
    vis[n][fold][bound] = true;
    if (n == 0) return &(memo[n][fold][bound]);
    if (n == 1) {
        memo[n][fold][bound].insert(zero);
        memo[n][fold][bound].insert(one);
        memo[n][fold][bound].insert(x);
        if (bound) {
            memo[n][fold][bound].insert(y);
            memo[n][fold][bound].insert(z);
        }
    }
    if (n >= 2) {
        //op1
        for (int i = 0; i < op1s.size(); i++) {
            exprset* e = enumerate_expr(n - 1, fold, bound);
            for (exprset::iterator it = e->begin(); it != e->end(); ++it) {
                SyntaxTree expr(op1s[i], 0, &(*it));
                if (!can_optimize_expr(expr)) memo[n][fold][bound].insert(expr);
            }
        }
    }

    if (n >= 3) {
        //op2
        for (int i = 0; i < op2s.size(); i++) {
            for (int j = 1; j < n - 1; j++) {
                exprset* e1 = enumerate_expr(j, fold, bound);
                exprset* e2 = enumerate_expr(n - j - 1, false, bound);
                for (exprset::iterator it1 = e1->begin(); it1 != e1->end(); ++it1) {
                    for (exprset::iterator it2 = e2->begin(); it2 != e2->end(); ++it2) {
                        SyntaxTree expr(op2s[i], 0, &(*it1), &(*it2));
                        if (!can_optimize_expr(expr)) memo[n][fold][bound].insert(expr);
                    }
                }
                e1 = enumerate_expr(j, false, bound);
                e2 = enumerate_expr(n - j - 1, fold, bound);
                for (exprset::iterator it1 = e1->begin(); it1 != e1->end(); ++it1) {
                    for (exprset::iterator it2 = e2->begin(); it2 != e2->end(); ++it2) {
                        SyntaxTree expr(op2s[i], 0, &(*it1), &(*it2));
                        if (!can_optimize_expr(expr)) memo[n][fold][bound].insert(expr);
                    }
                }
            }
        }
    }

    //if0
    if (n >= 4 && if0f) {
        for (int j = 1; j < n - 2; j++) {
            for (int k = 1; j + k < n - 1; k++) {
                exprset* e1 = enumerate_expr(j, fold, bound);
                exprset* e2 = enumerate_expr(k, false, bound);
                exprset* e3 = enumerate_expr(n - j - k - 1, false, bound);
                for (exprset::iterator it1 = e1->begin(); it1 != e1->end(); ++it1) {
                    for (exprset::iterator it2 = e2->begin(); it2 != e2->end(); ++it2) {
                        for (exprset::iterator it3 = e3->begin(); it3 != e3->end(); ++it3) {
                            SyntaxTree expr(IF0, 0, &(*it1), &(*it2), &(*it3));
                            if (!can_optimize_expr(expr)) memo[n][fold][bound].insert(expr);
                        }
                    }
                }
                e1 = enumerate_expr(j, false, bound);
                e2 = enumerate_expr(k, fold, bound);
                e3 = enumerate_expr(n - j - k - 1, false, bound);
                for (exprset::iterator it1 = e1->begin(); it1 != e1->end(); ++it1) {
                    for (exprset::iterator it2 = e2->begin(); it2 != e2->end(); ++it2) {
                        for (exprset::iterator it3 = e3->begin(); it3 != e3->end(); ++it3) {
                            SyntaxTree expr(IF0, 0, &(*it1), &(*it2), &(*it3));
                            if (!can_optimize_expr(expr)) memo[n][fold][bound].insert(expr);
                        }
                    }
                }
                e1 = enumerate_expr(j, false, bound);
                e2 = enumerate_expr(k, false, bound);
                e3 = enumerate_expr(n - j - k - 1, fold, bound);
                for (exprset::iterator it1 = e1->begin(); it1 != e1->end(); ++it1) {
                    for (exprset::iterator it2 = e2->begin(); it2 != e2->end(); ++it2) {
                        for (exprset::iterator it3 = e3->begin(); it3 != e3->end(); ++it3) {
                            SyntaxTree expr(IF0, 0, &(*it1), &(*it2), &(*it3));
                            if (!can_optimize_expr(expr)) memo[n][fold][bound].insert(expr);
                        }
                    }
                }
            }
        }
    }

    //fold
    if (n >= 5 && foldf && fold) {
         for (int j = 1; j < n - 3; j++) {
            for (int k = 1; j + k < n - 2; k++) {
                exprset* e1 = enumerate_expr(j, false, false);
                exprset* e2 = enumerate_expr(k, false, false);
                exprset* e3 = enumerate_expr(n - j - k - 2, false, true);
                for (exprset::iterator it1 = e1->begin(); it1 != e1->end(); ++it1) {
                    for (exprset::iterator it2 = e2->begin(); it2 != e2->end(); ++it2) {
                        for (exprset::iterator it3 = e3->begin(); it3 != e3->end(); ++it3) {
                            SyntaxTree expr(FOLD, 0, &(*it1), &(*it2), &(*it3));
                            if (!can_optimize_expr(expr)) memo[n][fold][bound].insert(expr);
                        }
                    }
                }
            }
         }
    }

    return &(memo[n][fold][bound]);
}

int main() {
    map<string, Operation> op1cand = {{"not", NOT}, {"shl1", SHL1}, {"shr1", SHR1}, {"shr4", SHR4}, {"shr16", SHR16}};
    map<string, Operation> op2cand = {{"and", AND}, {"or", OR}, {"xor", XOR}, {"plus", PLUS}};

    int n, m;
    cin >> n >> m;
    for (int i = 0; i < m; i++) {
        string op;
        cin >> op;
        if (op1cand.find(op) != op1cand.end()) op1s.push_back(op1cand[op]);
        if (op2cand.find(op) != op2cand.end()) op2s.push_back(op2cand[op]);
        if (op == "if0") if0f = true;
        if (op == "fold") foldf = true;
        if (op == "tfold") foldf = true;
    }

    exprset e;
    for (int i = 1; i <= n - 1; i++) {
        exprset* tmp = enumerate_expr(n - 1, true, false);
        for (auto it = tmp->begin(); it != tmp->end(); ++it) {
            e.insert(*it);
        }
    }

    while (1) {
        cerr << "candidates: " << e.size() << endl;
        if (e.size() == 0) {
            cerr << "i will die" << endl;
            break;
        }
        SyntaxTree s = *(e.begin());
        e.erase(e.begin());
        cout << "guess (lambda (v0) " + s.toString() + ")" << endl;

        string response;
        cin >> response;
        if (response == "win") {
            cerr << "win!" << endl;
            break;
        }
        if (response == "unknown") {
            cerr << "unknown" << endl;
            continue;
        }
        if (response == "mismatch") {
            unsigned long long input, exout, myout;
            cin >> input >> exout >> myout;
            cerr << "Input: " << input << ", Expected Output:" << exout << endl;
            exprset nexte;
            for (exprset::iterator it = e.begin(); it != e.end(); ++it) {
                if (it->eval(input) == exout) nexte.insert(*it);
            }
            e = nexte;
        }
    }
    return 0;
}
