//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include "cool.h"
#include "stringtab.h"
#include "symtab.h"
#include "tree.h"
#include <iostream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#define yylineno curr_lineno;
extern int yylineno;

template <typename K, typename V> using HashMap = std::unordered_map<K, V>;
// <Attr name, offset> Access with offset * WORD_SIZE($s0)
using AttrEnv = const HashMap<Symbol, int> &;
// Store the pos of variables except attrs.
using SEnv = SymbolTable<Symbol, int>*;

inline Boolean copy_Boolean(Boolean b) { return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream &stream, int padding, Boolean b) {
  stream << pad(padding) << (int)b << "\n";
}

void dump_Symbol(ostream &stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class Program_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

#define Program_EXTRAS                                                                                       \
  virtual void cgen(ostream &) = 0;                                                                          \
  virtual void dump_with_types(ostream &, int) = 0;

#define program_EXTRAS                                                                                       \
  void cgen(ostream &);                                                                                      \
  void dump_with_types(ostream &, int);

#define Class__EXTRAS                                                                                        \
  virtual Symbol get_name() = 0;                                                                             \
  virtual Symbol get_parent() = 0;                                                                           \
  virtual Symbol get_filename() = 0;                                                                         \
  virtual void dump_with_types(ostream &, int) = 0;

#define class__EXTRAS                                                                                        \
  Symbol get_name() { return name; }                                                                         \
  Symbol get_parent() { return parent; }                                                                     \
  Symbol get_filename() { return filename; }                                                                 \
  void dump_with_types(ostream &, int);

#define Feature_EXTRAS                                                                                       \
  virtual Symbol get_name() = 0;                                                                             \
  virtual void dump_with_types(ostream &, int) = 0;

// # of temporary variables created by let, type... 
#define Feature_SHARED_EXTRAS                                                                                \
private:      \
  int num_temp;   \
public:     \
  int get_num_temp() { return num_temp; }     \
  void cal_num_temp();     \
  void code(SEnv, int, AttrEnv, ostream &);                                                                              \
  Symbol get_name() override { return name; }                                                                \
  void dump_with_types(ostream &, int);

#define Formal_EXTRAS                                                                                        \
  virtual Symbol get_type_decl() = 0;                                                                        \
  virtual Symbol get_name() = 0;                                                                             \
  virtual void dump_with_types(ostream &, int) = 0;

#define formal_EXTRAS                                                                                        \
  Symbol get_type_decl() override { return type_decl; }                                                      \
  Symbol get_name() override { return name; }                                                                \
  void dump_with_types(ostream &, int);

#define Case_EXTRAS virtual void dump_with_types(ostream &, int) = 0;

#define branch_EXTRAS void dump_with_types(ostream &, int);

#define Expression_EXTRAS                                                                                    \
  Symbol type;                                                                                               \
  Symbol get_type() { return type; }                                                                         \
  Expression set_type(Symbol s) {                                                                            \
    type = s;                                                                                                \
    return this;                                                                                             \
  }                                                                                                          \
  virtual void code(SEnv, int, AttrEnv, ostream &) = 0;                                                                          \
  virtual int cal_num_temp() = 0;     \
  virtual void dump_with_types(ostream &, int) = 0;                                                          \
  void dump_type(ostream &, int);                                                                            \
  Expression_class() { type = (Symbol)NULL; }

#define Expression_SHARED_EXTRAS                                                                             \
  void code(SEnv, int, AttrEnv, ostream &);                                                                                      \
  void dump_with_types(ostream &, int);

#endif
