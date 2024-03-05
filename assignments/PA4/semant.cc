

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    install_basic_classes();

    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Symbol filename = classes->nth(i)->get_filename();
        Symbol name = classes->nth(i)->get_name();
        Symbol parent = classes->nth(i)->get_parent();
        int line_number = classes->nth(i)->get_line_number();

        if (name == Object || name == Int || name == Bool || name == Str || name == IO) {
            // Redefinition of basic class.
            ++semant_errors;
            error_stream << filename << ':' << line_number << ": " << "Redefinition of basic class " << name << ".\n";
            return;
        }
        
        add_class(classes->nth(i));
    }

    // if (!check_all_defined(classes) || !check_cycle(classes) || !check_main() || !check_consistent_method(classes)) {
    //     return;
    // }

    check_all_defined(classes);
    if (!check_cycle(classes)) {
        return;
    }
    check_main();
    check_consistent_method();
    check_consistent_attr();

    // Start to type inference.
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Symbol name = classes->nth(i)->get_name();
        MethodEnv method_env = make_method_env(name);
        O_Env o_env = make_o_env(name);
        classes->nth(i)->type_infer(o_env, method_env);
    }
}

bool ClassTable::check_consistent_method() {
    bool flag = true;
    for (const auto& [c, methods]: class_methods) {
        for (const auto& [m_name, types]: methods) {
            Symbol name = c;
            // If has no parent class, then must be correct.
            while (has_parent(name)) {
                Symbol parent = dependency[name];
                const auto& parent_methods = class_methods.at(parent);
                if (parent_methods.find(m_name) != parent_methods.end()) {
                    const auto& parent_method_types = parent_methods.at(m_name);
                    bool check = (types.size() == parent_method_types.size());
                    if (check) {
                        for (size_t i = 0; i < types.size(); i++) {
                            if (types[i] != parent_method_types[i]) {
                                check = false;
                                break;
                            }
                        }
                    }
                    if (!check) {
                        ++semant_errors;
                        error_stream << get_class_file(name) << ':' << get_class_method_line(name, m_name) << ": " 
                        << "Redefined method " << m_name <<" has different argument types.\n";
                        flag = false;
                    }
                }
                name = parent;
            }
        }
    }
    return flag;
}

bool ClassTable::check_consistent_attr() {
    bool flag = true;
    for (const auto& [c, attrs]: class_attrs) {
        for (const auto& [a_name, type]: attrs) {
            Symbol name = c;
            while (has_parent(name)) {
                Symbol parent = dependency[name];
                const auto& parent_attrs = class_attrs.at(parent);
                if (parent_attrs.find(a_name) != parent_attrs.end()) {
                    ++semant_errors;
                    error_stream << get_class_file(name) << ':' << get_class_attr_line(name, a_name) << ": " 
                    << "Attribute " << a_name <<" is an attribute of an inherited class.\n";
                    flag = false; 
                }
                name = parent;
            }
        }
    }
    return flag;
}

// This function can be merged into `check_cycle`. But for clarity, still seperate them. 
bool ClassTable::check_all_defined(Classes classes) {
    bool flag = true;
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Symbol name = classes->nth(i)->get_name();

        if (dependency.find(dependency[name]) == dependency.end()) {
            ++semant_errors;
            error_stream << get_class_file(name) << ':' << get_class_line(name) << ": " << "Class " << name << 
            " inherits from an undefined class " << dependency[name] << ".\n";
            flag = false;
        }
    }
    return flag;
}

// This function will find the cycle and report it accurately.
// No need to check the basic class, because they will only be inherited.
bool ClassTable::check_cycle(Classes classes) {
    int flag = true;
    // 0 <- no detected | 1 <- detected | 2 <- reported
    // Introducing state 2 is to avoid reporting a same entrance repeatedly.
    // e.g.: A:1: Find an inheritance cycle: Class A < Class C < Class B < Class A
    //       C:7: Find an inheritance cycle: Class C < Class B < Class A < Class C
    HashMap<Symbol, int> detected{};
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) { 
        Symbol name = classes->nth(i)->get_name();

        if (detected[name] == 0) { 
            detected[name] = 1;
            // Ordinary methods
            Symbol quick = name, slow = name;
            while (quick && dependency[quick]) {
                if (detected[dependency[quick]] == 0) {
                    detected[dependency[quick]] = 1;
                }
                quick = dependency[dependency[quick]];
                if (detected[quick] == 0) {
                    detected[quick] = 1;
                }
                slow = dependency[slow];

                if (quick == slow) {
                    quick = name;
                    while (quick != slow) {
                        quick = dependency[quick];
                        slow = dependency[slow];
                    }
                    if (detected[quick] != 2) {
                        ++semant_errors;
                        error_stream << get_class_file(quick) << ':' << get_class_line(quick) << ": " << "Find an inheritance cycle: ";
                        do {
                            detected[quick] = 2;
                            error_stream << "Class "<< quick << " < ";
                            quick = dependency[quick];
                        } while (quick != slow);
                        error_stream << "Class " << quick << "\n";
                    }
                    flag = false;
                    break;
                }
            }
        }
    }
    return flag;
}

bool ClassTable::check_main() {
    if (dependency.find(Main) == dependency.end()) {
        ++semant_errors;
        error_stream << "Class Main is not defined.\n";
        return false;
    }
    return true;
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

    add_class(Object_class);
    add_class(IO_class);
    add_class(Bool_class);
    add_class(Str_class);
    add_class(Int_class);
}

void ClassTable::add_class(Class_ c) {
    Symbol name = c->get_name();
    Symbol parent = c->get_parent();
    Symbol filename = c->get_filename();
    int line_number = c->get_line_number();
    
    c->collect_info();
    if (c->get_error_msg() != "") {
        ++semant_errors;
        error_stream << c->get_error_msg();
    }

    if (dependency.find(name) != dependency.end()) {
            // Multiple definitions error
            ++semant_errors = 2;
            error_stream << filename << ':' << line_number << ": " << "Class " << name << " was previously defined.\n";
            return;
    }

    dependency[name] = parent;
    class_methods.emplace(name, c->get_methods());
    class_attrs.emplace(name, c->get_attrs());
    classes.emplace(name, c);
}

MethodEnv ClassTable::make_method_env(Symbol name) {
    // Don't care about delete.
    auto* method_env = new std::vector<std::pair<Symbol, const HashMap<Symbol, const std::vector<Symbol>&>& >>();
    while (has_parent(name)) {
        method_env->emplace_back(name, class_methods.at(name));
        name = dependency[name];
    }
    return *method_env;
}

O_Env ClassTable::make_o_env(Symbol name) {
    auto* o_env = new SymbolTable<Symbol, Symbol>();
    o_env->enterscope();
    while (has_parent(name)) {
        auto& attrs = class_attrs.at(name);
        for (auto&[a_name, type]: attrs) {
            // The interface does not accept const. F**K
            o_env->addid(a_name, &const_cast<Symbol&>(type));
        }
        name = dependency[name];
    } 
    return *o_env;
}

bool ClassTable::has_parent(Symbol name) {
    return name != Object;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}


////////////////////////////////////////////////////////////////////
//
//      Class__class and class__class
//
////////////////////////////////////////////////////////////////////

void class__class::collect_info() {
    // We have to dynamic_cast to collect the info of methods and attrs.
    // We also check the error of them.
    for(int i = features->first(); features->more(i); i = features->next(i)) { 
        method_class* method = dynamic_cast<method_class*>(features->nth(i));
        if (method != nullptr) {
            method->register_class(this);
            method->check_error();
            add_method(method);
            method_line.emplace(method->get_name(), method->get_line_number());
        } else {
            attr_class* attr = dynamic_cast<attr_class*>(features->nth(i));
            if (attr != nullptr) {
                attr->register_class(this);
                add_attr(attr);
                attr_line.emplace(attr->get_name(), attr->get_line_number());
            }
        }
    }
}

void class__class::type_infer(O_Env o_env, MethodEnv m_env) {
    for(int i = features->first(); features->more(i); i = features->next(i)) {
        features->nth(i)->type_infer(o_env, m_env);
    }
}

void Class__class::add_method(method_class* method) {
    Symbol name = method->get_name();
    const auto& types = method->collect_type();
    if (methods.find(name) != methods.end()) {
        error_msg += std::string(get_filename()->get_string()) + ':' + method->gen_multiple_def_error();
        return;
    } 
    methods.emplace(name, types);
}

void Class__class::add_attr(attr_class* attr) {
    Symbol name = attr->get_name();
    Symbol type = attr->collect_type();
    if (attrs.find(name) != attrs.end()) {
        error_msg += std::string(get_filename()->get_string()) + ':' + attr->gen_multiple_def_error();
        return;
    }
    attrs[name] = type;
}

void Class__class::receive_error(std::string error) {
    error_msg += std::string(get_filename()->get_string()) + ':' + error;
}

////////////////////////////////////////////////////////////////////
//
//      Feature_class, method_class and attr_class
//
////////////////////////////////////////////////////////////////////

const std::vector<Symbol>& method_class::collect_type() {
    for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
        types.emplace_back(formals->nth(i)->get_type_decl());   
    }
    types.emplace_back(return_type);
    return types;
}

void method_class::check_error() {
    check_unique();
}

void method_class::check_unique() {
    std::unordered_set<Symbol> names;
    for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Symbol name = formals->nth(i)->get_name();
        int line_number = formals->nth(i)->get_line_number();

        if (names.find(name) != names.end()) {
            notify_error(std::to_string(line_number) + " Formal parameter " + 
            name->get_string() + " is multiply defined.\n");
        } else {
            names.emplace(name);
        }
    }
}

void method_class::type_infer(O_Env o_env, MethodEnv m_env) {
    o_env.enterscope();
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Symbol name = formals->nth(i)->get_name();
        o_env.addid(name, &types[i]);
    }
    Symbol expr_type = expr->type_infer(o_env, m_env);
    o_env.exitscope();
}

void attr_class::type_infer(O_Env o_env, MethodEnv m_env) {}
