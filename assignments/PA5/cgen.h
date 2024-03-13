#include "cool-tree.h"
#include "emit.h"
#include "symtab.h"
#include <assert.h>
#include <stdio.h>

enum Basicness { Basic, NotBasic };
#define TRUE 1
#define FALSE 0

#define INVALID_TAG -2
#define INVALID_SIZE -1

// Because the particularity of the three basic const class
// we have to assign the tag index starting at 5.
#define START_TAG_INDEX 5

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol, CgenNode> {
private:
  List<CgenNode> *nds;
  ostream &str;
  // Object 0 | Str 1 | Int 2 | Bool 3 | IO 4 | -1 for SELF_TYPE, No_Class...
  int stringclasstag;
  int intclasstag;
  int boolclasstag;

  int tag_index = START_TAG_INDEX;

  int get_next_tag() { return tag_index++; }

  // The following methods emit code for
  // constants and global declarations.

  void code_global_data();
  void code_global_text();
  void code_bools(int);
  void code_select_gc();
  void code_constants();

  void code_prototype();
  void code_class_nameTab();
  void code_dispatchTab();

  // The following creates an inheritance graph from
  // a list of classes.  The graph is implemented as
  // a tree of `CgenNode', and class names are placed
  // in the base class symbol table.

  void install_basic_classes();
  void install_class(CgenNodeP nd);
  void install_classes(Classes cs);
  void build_inheritance_tree();
  void set_relations(CgenNodeP nd);

public:
  CgenClassTable(Classes, ostream &str);
  void code();
  CgenNodeP root();
};

class CgenNode : public class__class {
private:
  CgenNodeP parentnd;       // Parent of class
  List<CgenNode> *children; // Children of class
  Basicness basic_status;   // `Basic' if class is basic
                            // `NotBasic' otherwise
  int class_tag = INVALID_TAG;
  int proto_size = INVALID_SIZE;
  // methods <Method m, arg Type 0, arg Type 1...>
  // attrs <Method m, Type t>
  // For the iteration order, use std::vector instead of HashMap
  std::vector<std::pair<Symbol, const std::vector<Symbol> &>> methods{};
  std::vector<std::pair<Symbol, Symbol>> attrs{};

public:
  CgenNode(Class_ c, Basicness bstatus, CgenClassTableP class_table, int tag);

  void add_child(CgenNodeP child);
  List<CgenNode> *get_children() { return children; }
  void set_parentnd(CgenNodeP p);
  CgenNodeP get_parentnd() { return parentnd; }
  int basic() { return (basic_status == Basic); }
  int get_tag() const;

  // Collect Info of methods and attrs
  void collect_info();
  void add_method(method_class *);
  void add_attr(attr_class *);

  // Calculate the prototype size.
  void cal_proto_size();
  int get_proto_size() { return proto_size; }
  int get_attrs_size() { return attrs.size(); }

  void emit_methods(ostream &str);
  void emit_default_attrs(ostream &str);
};

class BoolConst {
private:
  int val;

public:
  BoolConst(int);
  void code_def(ostream &, int boolclasstag);
  void code_ref(ostream &) const;
};
