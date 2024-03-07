#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0


// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();

  bool check_all_defined(Classes);
  bool check_cycle(Classes);
  bool check_main();
  bool check_consistent_method();
  bool check_consistent_attr();

  ostream& error_stream;

  // Prepare the env. Though these infos can be asked by using classes, store them for speed.
  // Because they are all pointers or references, it will not waste too much space.
  HashMap<Symbol, Symbol> dependency{};
  HashMap<Symbol, const HashMap<Symbol, const std::vector<Symbol>&>&> class_methods{};
  HashMap<Symbol, const HashMap<Symbol, Symbol>&> class_attrs{};

  HashMap<Symbol, Class_> classes;
  void add_class(Class_ c);

  MethodEnv make_method_env(Symbol name);
  O_Env make_o_env(Symbol name);

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  void receive_error(std::string error_msg)  { error_stream << error_msg; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

  void type_infer(Classes);

  bool has_parent(Symbol name);

  // To report the info more accurately.(e.g. what is #line of the class at the entrance in check_cycle)
  int get_class_line(Symbol name) {
    return classes.at(name)->get_line_number();
  }

  int get_class_method_line(Symbol name, Symbol m_name) {
    return classes.at(name)->get_method_line(m_name);
  }

  int get_class_attr_line(Symbol name, Symbol a_name) {
    return classes.at(name)->get_attr_line(a_name);
  }

  std::string get_class_file(Symbol name) {
    return std::string(classes.at(name)->get_filename()->get_string());
  }
};

static bool check_type_conform(DepEnv dep_env, Symbol type_1, Symbol type_2, Symbol C);
static Symbol find_least_upper_bound(DepEnv dep_env, Symbol type_1, Symbol type_2, Symbol C);
static Symbol handle_SELF_TYPE(Symbol type, Symbol C);


#endif

