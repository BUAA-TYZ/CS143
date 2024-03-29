#ifndef COOL_TREE_H
#define COOL_TREE_H
//////////////////////////////////////////////////////////
//
// file: cool-tree.h
//
// This file defines classes for each phylum and constructor
//
//////////////////////////////////////////////////////////

#include "cool-tree.handcode.h"
#include "tree.h"

static int max(int x, int y) { return x < y ? y : x; }

// define the class for phylum
// define simple phylum - Program
typedef class Program_class *Program;

class Program_class : public tree_node {
public:
  tree_node *copy() { return copy_Program(); }
  virtual Program copy_Program() = 0;

#ifdef Program_EXTRAS
  Program_EXTRAS
#endif
};

// define simple phylum - Class_
typedef class Class__class *Class_;

class Class__class : public tree_node {
public:
  tree_node *copy() { return copy_Class_(); }
  virtual Class_ copy_Class_() = 0;

#ifdef Class__EXTRAS
  Class__EXTRAS
#endif
};

// define simple phylum - Feature
typedef class Feature_class *Feature;

class Feature_class : public tree_node {
public:
  tree_node *copy() { return copy_Feature(); }
  virtual Feature copy_Feature() = 0;

#ifdef Feature_EXTRAS
  Feature_EXTRAS
#endif
};

// define simple phylum - Formal
typedef class Formal_class *Formal;

class Formal_class : public tree_node {
public:
  tree_node *copy() { return copy_Formal(); }
  virtual Formal copy_Formal() = 0;

#ifdef Formal_EXTRAS
  Formal_EXTRAS
#endif
};

// define simple phylum - Expression
typedef class Expression_class *Expression;

class Expression_class : public tree_node {
public:
  tree_node *copy() { return copy_Expression(); }
  virtual Expression copy_Expression() = 0;

#ifdef Expression_EXTRAS
  Expression_EXTRAS
#endif
};

// define simple phylum - Case
typedef class Case_class *Case;

class Case_class : public tree_node {
public:
  tree_node *copy() { return copy_Case(); }
  virtual Case copy_Case() = 0;

#ifdef Case_EXTRAS
  Case_EXTRAS
#endif
};

// define the class for phylum - LIST
// define list phlyum - Classes
typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;

// define list phlyum - Features
typedef list_node<Feature> Features_class;
typedef Features_class *Features;

// define list phlyum - Formals
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;

// define list phlyum - Expressions
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;

// define list phlyum - Cases
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

// define the class for constructors
// define constructor - program
class program_class : public Program_class {
public:
  Classes classes;

public:
  program_class(Classes a1) { classes = a1; }
  Program copy_Program();
  void dump(ostream &stream, int n);

#ifdef program_EXTRAS
  program_EXTRAS
#endif
};

// define constructor - class_
class class__class : public Class__class {
public:
  Symbol name;
  Symbol parent;
  Features features;
  Symbol filename;

public:
  class__class(Symbol a1, Symbol a2, Features a3, Symbol a4) {
    name = a1;
    parent = a2;
    features = a3;
    filename = a4;
  }
  Class_ copy_Class_();
  void dump(ostream &stream, int n);

#ifdef class__EXTRAS
  class__EXTRAS
#endif
};

// define constructor - method
class method_class : public Feature_class {
public:
  Symbol name;
  Formals formals;
  Symbol return_type;
  Expression expr;
  std::vector<Symbol> types;

public:
  method_class(Symbol a1, Formals a2, Symbol a3, Expression a4) {
    name = a1;
    formals = a2;
    return_type = a3;
    expr = a4;
  }
  Feature copy_Feature();
  void dump(ostream &stream, int n);

  const std::vector<Symbol> &collect_type();

#ifdef Feature_SHARED_EXTRAS
  Feature_SHARED_EXTRAS
#endif
};

// define constructor - attr
class attr_class : public Feature_class {
public:
  Symbol name;
  Symbol type_decl;
  Expression init;

public:
  attr_class(Symbol a1, Symbol a2, Expression a3) {
    name = a1;
    type_decl = a2;
    init = a3;
  }
  Feature copy_Feature();
  void dump(ostream &stream, int n);

  Symbol collect_type() { return type_decl; }
  bool has_init_expr();

#ifdef Feature_SHARED_EXTRAS
  Feature_SHARED_EXTRAS
#endif
};

// define constructor - formal
class formal_class : public Formal_class {
public:
  Symbol name;
  Symbol type_decl;

public:
  formal_class(Symbol a1, Symbol a2) {
    name = a1;
    type_decl = a2;
  }
  Formal copy_Formal();
  void dump(ostream &stream, int n);

#ifdef formal_EXTRAS
  formal_EXTRAS
#endif
};

// define constructor - branch
class branch_class : public Case_class {
public:
  Symbol name;
  Symbol type_decl;
  Expression expr;

public:
  branch_class(Symbol a1, Symbol a2, Expression a3) {
    name = a1;
    type_decl = a2;
    expr = a3;
  }
  Case copy_Case();
  void dump(ostream &stream, int n);
  int cal_num_temp() { return expr->cal_num_temp(); }

#ifdef branch_EXTRAS
  branch_EXTRAS
#endif
};

// define constructor - assign
class assign_class : public Expression_class {
public:
  Symbol name;
  Expression expr;

public:
  assign_class(Symbol a1, Expression a2) {
    name = a1;
    expr = a2;
  }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return expr->cal_num_temp(); }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - static_dispatch
class static_dispatch_class : public Expression_class {
public:
  Expression expr;
  Symbol type_name;
  Symbol name;
  Expressions actual;

public:
  static_dispatch_class(Expression a1, Symbol a2, Symbol a3, Expressions a4) {
    expr = a1;
    type_name = a2;
    name = a3;
    actual = a4;
  }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return expr->cal_num_temp(); }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - dispatch
class dispatch_class : public Expression_class {
public:
  Expression expr;
  Symbol name;
  Expressions actual;

public:
  dispatch_class(Expression a1, Symbol a2, Expressions a3) {
    expr = a1;
    name = a2;
    actual = a3;
  }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return expr->cal_num_temp(); }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - cond
class cond_class : public Expression_class {
public:
  Expression pred;
  Expression then_exp;
  Expression else_exp;

public:
  cond_class(Expression a1, Expression a2, Expression a3) {
    pred = a1;
    then_exp = a2;
    else_exp = a3;
  }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override {
    return max(pred->cal_num_temp(), max(then_exp->cal_num_temp(), else_exp->cal_num_temp()));
  }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - loop
class loop_class : public Expression_class {
public:
  Expression pred;
  Expression body;

public:
  loop_class(Expression a1, Expression a2) {
    pred = a1;
    body = a2;
  }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return max(pred->cal_num_temp(), body->cal_num_temp()); }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - typcase
class typcase_class : public Expression_class {
public:
  Expression expr;
  Cases cases;

public:
  typcase_class(Expression a1, Cases a2) {
    expr = a1;
    cases = a2;
  }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override {
    int i = cases->first();
    int cases_need = cases->nth(i)->cal_num_temp();
    i = cases->next(i);
    for (; cases->more(i); i = cases->next(i)) {
      cases_need = max(cases_need, cases->nth(i)->cal_num_temp());
    }
    // Cases need one.
    return max(1, expr->cal_num_temp()) + cases_need;
  }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - block
class block_class : public Expression_class {
public:
  Expressions body;

public:
  block_class(Expressions a1) { body = a1; }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override {
    int i = body->first();
    int res = body->nth(i)->cal_num_temp();
    i = body->next(i);
    for (; body->more(i); i = body->next(i)) {
      res = max(res, body->nth(i)->cal_num_temp());
    }
    return res;
  }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - let
class let_class : public Expression_class {
public:
  Symbol identifier;
  Symbol type_decl;
  Expression init;
  Expression body;

public:
  let_class(Symbol a1, Symbol a2, Expression a3, Expression a4) {
    identifier = a1;
    type_decl = a2;
    init = a3;
    body = a4;
  }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return 1 + max(init->cal_num_temp(), body->cal_num_temp()); }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - plus
class plus_class : public Expression_class {
public:
  Expression e1;
  Expression e2;

public:
  plus_class(Expression a1, Expression a2) {
    e1 = a1;
    e2 = a2;
  }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return max(e1->cal_num_temp(), e2->cal_num_temp()); }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - sub
class sub_class : public Expression_class {
public:
  Expression e1;
  Expression e2;

public:
  sub_class(Expression a1, Expression a2) {
    e1 = a1;
    e2 = a2;
  }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return max(e1->cal_num_temp(), e2->cal_num_temp()); }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - mul
class mul_class : public Expression_class {
public:
  Expression e1;
  Expression e2;

public:
  mul_class(Expression a1, Expression a2) {
    e1 = a1;
    e2 = a2;
  }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return max(e1->cal_num_temp(), e2->cal_num_temp()); }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - divide
class divide_class : public Expression_class {
public:
  Expression e1;
  Expression e2;

public:
  divide_class(Expression a1, Expression a2) {
    e1 = a1;
    e2 = a2;
  }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return max(e1->cal_num_temp(), e2->cal_num_temp()); }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - neg
class neg_class : public Expression_class {
public:
  Expression e1;

public:
  neg_class(Expression a1) { e1 = a1; }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return e1->cal_num_temp(); }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - lt
class lt_class : public Expression_class {
public:
  Expression e1;
  Expression e2;

public:
  lt_class(Expression a1, Expression a2) {
    e1 = a1;
    e2 = a2;
  }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return max(e1->cal_num_temp(), e2->cal_num_temp()); }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - eq
class eq_class : public Expression_class {
public:
  Expression e1;
  Expression e2;

public:
  eq_class(Expression a1, Expression a2) {
    e1 = a1;
    e2 = a2;
  }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return max(e1->cal_num_temp(), e2->cal_num_temp()); }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - leq
class leq_class : public Expression_class {
public:
  Expression e1;
  Expression e2;

public:
  leq_class(Expression a1, Expression a2) {
    e1 = a1;
    e2 = a2;
  }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return max(e1->cal_num_temp(), e2->cal_num_temp()); }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - comp
class comp_class : public Expression_class {
public:
  Expression e1;

public:
  comp_class(Expression a1) { e1 = a1; }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return e1->cal_num_temp(); }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - int_const
class int_const_class : public Expression_class {
public:
  Symbol token;

public:
  int_const_class(Symbol a1) { token = a1; }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return 0; }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - bool_const
class bool_const_class : public Expression_class {
public:
  Boolean val;

public:
  bool_const_class(Boolean a1) { val = a1; }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return 0; }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - string_const
class string_const_class : public Expression_class {
public:
  Symbol token;

public:
  string_const_class(Symbol a1) { token = a1; }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return 0; }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - new_
class new__class : public Expression_class {
public:
  Symbol type_name;

public:
  new__class(Symbol a1) { type_name = a1; }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return 0; }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - isvoid
class isvoid_class : public Expression_class {
public:
  Expression e1;

public:
  isvoid_class(Expression a1) { e1 = a1; }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return e1->cal_num_temp(); }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - no_expr
class no_expr_class : public Expression_class {
public:
public:
  no_expr_class() {}
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return 0; }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define constructor - object
class object_class : public Expression_class {
public:
  Symbol name;

public:
  object_class(Symbol a1) { name = a1; }
  Expression copy_Expression();
  void dump(ostream &stream, int n);
  int cal_num_temp() override { return 0; }

#ifdef Expression_SHARED_EXTRAS
  Expression_SHARED_EXTRAS
#endif
};

// define the prototypes of the interface
Classes nil_Classes();
Classes single_Classes(Class_);
Classes append_Classes(Classes, Classes);
Features nil_Features();
Features single_Features(Feature);
Features append_Features(Features, Features);
Formals nil_Formals();
Formals single_Formals(Formal);
Formals append_Formals(Formals, Formals);
Expressions nil_Expressions();
Expressions single_Expressions(Expression);
Expressions append_Expressions(Expressions, Expressions);
Cases nil_Cases();
Cases single_Cases(Case);
Cases append_Cases(Cases, Cases);
Program program(Classes);
Class_ class_(Symbol, Symbol, Features, Symbol);
Feature method(Symbol, Formals, Symbol, Expression);
Feature attr(Symbol, Symbol, Expression);
Formal formal(Symbol, Symbol);
Case branch(Symbol, Symbol, Expression);
Expression assign(Symbol, Expression);
Expression static_dispatch(Expression, Symbol, Symbol, Expressions);
Expression dispatch(Expression, Symbol, Expressions);
Expression cond(Expression, Expression, Expression);
Expression loop(Expression, Expression);
Expression typcase(Expression, Cases);
Expression block(Expressions);
Expression let(Symbol, Symbol, Expression, Expression);
Expression plus(Expression, Expression);
Expression sub(Expression, Expression);
Expression mul(Expression, Expression);
Expression divide(Expression, Expression);
Expression neg(Expression);
Expression lt(Expression, Expression);
Expression eq(Expression, Expression);
Expression leq(Expression, Expression);
Expression comp(Expression);
Expression int_const(Symbol);
Expression bool_const(Boolean);
Expression string_const(Symbol);
Expression new_(Symbol);
Expression isvoid(Expression);
Expression no_expr();
Expression object(Symbol);

#endif
