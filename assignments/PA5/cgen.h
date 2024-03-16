#include "cool-tree.h"
#include "emit.h"
#include "symtab.h"
#include <assert.h>
#include <deque>
#include <stdio.h>

enum Basicness { Basic, NotBasic };
#define TRUE 1
#define FALSE 0

#define INVALID_TAG -2
#define INVALID_SIZE -1

// Because the particularity of the three basic const class
// we have to assign the tag index starting at 5.
#define START_TAG_INDEX 5

// Label index must be a global variable.
int label_index = 0;

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol, CgenNode> {
private:
  List<CgenNode> *nds;
  ostream &str;
  // Object 0 | IO 1 | Int 2 | Bool 3 | Str 4 | -1 for SELF_TYPE, No_Class...
  int stringclasstag;
  int intclasstag;
  int boolclasstag;

  int tag_index = START_TAG_INDEX;

  HashMap<Symbol, const HashMap<Symbol, int> &> class_m_pos{};

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
  void code_class_nameTab(List<CgenNode> *l);
  void code_class_objTab();
  void code_dispatchTab();

  void code_initializer(MEnv);
  void code_methods(MEnv);

  // The following creates an inheritance graph from
  // a list of classes.  The graph is implemented as
  // a tree of `CgenNode', and class names are placed
  // in the base class symbol table.

  void install_basic_classes();
  void install_class(CgenNodeP nd);
  void install_classes(Classes cs);
  void build_inheritance_tree();
  void set_relations(CgenNodeP nd);

  void collect_pos();

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

  // attr_start_index = proto_size - attrs.size()
  int proto_size = INVALID_SIZE;
  // methods <Method m, arg Type 0, arg Type 1...>
  // attrs <Method m, Type t>
  // For the iteration order, use std::vector instead of HashMap
  std::vector<std::pair<Symbol, method_class*>> methods{};
  std::vector<std::pair<Symbol, attr_class*>> attrs{};

  HashMap<Symbol, int> attrs_pos{};
  HashMap<Symbol, int> m_pos{};

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

  // Collect offset of attrs and methods
  void collect_pos();
  const HashMap<Symbol, int> & get_methods_pos() { return m_pos; }
  const HashMap<Symbol, int> & get_attrs_pos() { return attrs_pos; }
  HashMap<Symbol, int> inherit_attrs_pos() { return attrs_pos; }
  HashMap<Symbol, int> inherit_methods_pos() { return m_pos; }

  // Calculate the prototype size.
  void cal_proto_size();
  int get_proto_size() { return proto_size; }
  int get_attrs_size() { return attrs.size(); }

  void emit_methods(ostream &str);
  void emit_default_attrs(ostream &str);

  void emit_init(MEnv m_env, ostream &str);
  void emit_method_def(MEnv m_env, ostream &str);
};

class BoolConst {
private:
  int val;

public:
  BoolConst(int);
  void code_def(ostream &, int boolclasstag);
  void code_ref(ostream &) const;
};

static Symbol handle_SELF_TYPE(Symbol type, Symbol C);
