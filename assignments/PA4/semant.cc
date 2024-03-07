

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
        Class_ c = classes->nth(i);
        Symbol filename = c->get_filename();
        Symbol name = c->get_name();
        Symbol parent = c->get_parent();
        int line_number = c->get_line_number();

        if (name == Object || name == Int || name == Bool || name == Str || name == IO || name == SELF_TYPE) {
            // Redefinition of basic class.
            ++semant_errors;
            error_stream << filename << ':' << line_number << ": " << "Redefinition of basic class " << name << ".\n";
            return;
        }
        
        add_class(c);

        // Ensure that all infos are collected.
        if (semant_errors != 0) {
            return;
        }
    }

    // Ensure that all classes are defined(otherwise trigger out_of_range) and have no cycle(otherwise dead-loop).
    if (!check_all_defined(classes) || !check_cycle(classes)) {
        return;
    }

    check_main();
    check_consistent_method();
    check_consistent_attr();

    // Start to type inference.
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ c = classes->nth(i);
        Symbol name = c->get_name();
        MethodEnv method_env = make_method_env(name);
        O_Env o_env = make_o_env(name);
        c->type_infer(o_env, method_env, dependency, name);
        delete(&o_env);

        if (c->get_error_msg() != "") {
            ++semant_errors;
            error_stream << c->get_error_msg();
        }
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
    const auto& main_methods = class_methods.at(Main);
    if (main_methods.find(main_meth) == main_methods.end()) {
        ++semant_errors;
        error_stream << get_class_file(Main) << ":" << get_class_line(Main) << ": No 'main' method in class Main.\n";
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
    c->clear_error_msg();

    if (dependency.find(name) != dependency.end()) {
        // Multiple definitions error
        ++semant_errors;
        error_stream << filename << ':' << line_number << ": " << "Class " << name << " was previously defined.\n";
        return;
    }

    if (parent == Int || parent == Bool || parent == Str || parent == SELF_TYPE) {
        ++semant_errors;
        error_stream << filename << ':' << line_number << ": " << 
        "Class " << name << " cannot inherit class " << parent << ".\n";
        return;
    }

    dependency[name] = parent;
    class_methods.emplace(name, c->get_methods());
    class_attrs.emplace(name, c->get_attrs());
    classes.emplace(name, c);
}

MethodEnv ClassTable::make_method_env(Symbol name) {
    return class_methods;
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
        auto f = features->nth(i);
        method_class* method = dynamic_cast<method_class*>(f);
        if (method != nullptr) {
            method->register_class(this);
            method->check_error();
            add_method(method);
            method_line.emplace(method->get_name(), method->get_line_number());
        } else {
            attr_class* attr = dynamic_cast<attr_class*>(f);
            if (attr != nullptr) {
                attr->register_class(this);
                add_attr(attr);
                attr_line.emplace(attr->get_name(), attr->get_line_number());
            }
        }
    }
}

void class__class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    for(int i = features->first(); features->more(i); i = features->next(i)) {
        features->nth(i)->type_infer(o_env, m_env, dep_env, C);
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
    if (name == self) {
        error_msg += std::string(get_filename()->get_string()) + ':' + 
        std::to_string(attr->get_line_number()) + ": 'self' cannot be the name of an attribute.\n";
        return;
    }
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
        auto f = formals->nth(i);
        Symbol formal_type = f->get_type_decl();
        if (formal_type == SELF_TYPE) {
            notify_error(std::to_string(f->get_line_number()) + ": Formal parameter" + 
            f->get_name()->get_string() + " cannot have type SELF_TYPE.\n"); 
        } else {
            types.emplace_back(formal_type);   
        }
    }
    types.emplace_back(return_type);
    return types;
}

void method_class::check_error() {
    std::unordered_set<Symbol> names;
    for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
        auto f = formals->nth(i);
        Symbol name = f->get_name();

        if (names.find(name) != names.end()) {
            notify_error(std::to_string(f->get_line_number()) + ": Formal parameter " + 
            name->get_string() + " is multiply defined.\n");
            return;
        } else {
            if (name == self) {
                notify_error(std::to_string(f->get_line_number()) + 
                ": 'self' cannot be the name of a formal parameter.\n"); 
            } else {
                names.emplace(name);
            }
        }
    }
}

void method_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    o_env.enterscope();
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Symbol name = formals->nth(i)->get_name();
        o_env.addid(name, &types[i]);
    }
    o_env.addid(self, &SELF_TYPE);
    Symbol expr_type = expr->type_infer(o_env, m_env, dep_env, C);
    if (expr_type != SELF_TYPE && dep_env.find(expr_type) == dep_env.end()) {
        notify_error(std::to_string(get_line_number()) + 
        ": Undefined return type " + expr_type->get_string() + " in method " + name->get_string() + ".\n");
    }
    if (!check_type_conform(dep_env, expr_type, return_type, C)) {
        notify_error(std::to_string(get_line_number()) + 
        ": Inferred return type " + expr_type->get_string() + " of method " + name->get_string() + 
        " does not conform to declared return type " + return_type->get_string() + ".\n");
    }
    o_env.exitscope();
}

void attr_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    // Have to know in advance whether it should be init.
    if (dynamic_cast<no_expr_class*>(init) != nullptr) {
        return;
    }
    // Attr-Init
    o_env.enterscope();
    o_env.addid(self, &SELF_TYPE);
    Symbol init_type = init->type_infer(o_env, m_env, dep_env, C);
    if (!check_type_conform(dep_env, init_type, type_decl, C)) {
        notify_error(std::to_string(get_line_number()) + ": Inferred type " + 
        init_type->get_string() + " of initialization of attribute " + name->get_string() + 
        " does not conform to declared type " + type_decl->get_string() + ".\n");
    }
    o_env.exitscope();
}

////////////////////////////////////////////////////////////////////
//
//      Expression_class
//
////////////////////////////////////////////////////////////////////

Symbol assign_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    Symbol origin_type = Object;
    if (o_env.lookup(name) == NULL) {
        notify_error(std::to_string(get_line_number()) + 
        ": Assignment to undeclared variable " + name->get_string() + ".\n");
    } else {
        origin_type = *o_env.lookup(name);
        if (name == self) {
            notify_error(std::to_string(get_line_number()) + ": Cannot assign to 'self'.\n"); 
        }
    }
    Symbol res_type = expr->type_infer(o_env, m_env, dep_env, C);
    if (!check_type_conform(dep_env, res_type, origin_type, C)) {
        notify_error(std::to_string(get_line_number()) + ": Type " + res_type->get_string() + 
        " of assigned expression does not conform to declared type " + origin_type->get_string() + 
        " of identifier " + name->get_string() + ".\n");
        res_type = Object;
    }
    set_type(res_type);
    return res_type;
}

Symbol static_dispatch_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    Symbol init_type = expr->type_infer(o_env, m_env, dep_env, C);
    std::vector<Symbol> formal_types{};
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        formal_types.emplace_back(actual->nth(i)->type_infer(o_env, m_env, dep_env, C));
    } 
    if (m_env.count(type_name) == 0) {
        notify_error(std::to_string(get_line_number()) + 
        ": Static dispatch to undefined class " + type_name->get_string() + ".\n");
        return Object; 
    }
    if (!check_type_conform(dep_env, init_type, type_name, C)) {
        notify_error(std::to_string(get_line_number()) + ": Expression type " + init_type->get_string() + 
        " does not conform to declared static dispatch type " + type_name->get_string() + ".\n");  
        return Object; 
    }
    if (type_name == SELF_TYPE) {
        notify_error(std::to_string(get_line_number()) + ": Static dispatch to SELF_TYPE.\n");  
        return Object; 
    }
    Symbol cur = type_name;
    while (m_env.at(cur).count(name) == 0) {
        if (cur == Object) {
            notify_error(std::to_string(get_line_number()) + 
            ": Static dispatch to undefined method " + name->get_string() + ".\n");
            return Object;
        }
        cur = dep_env.at(cur);
    }
    const auto& method_types = m_env.at(cur).at(name);

    size_t n = formal_types.size();
    if (n != method_types.size() - 1) {
       notify_error(std::to_string(get_line_number()) + 
        ": Method " + name->get_string() + " called with wrong number of arguments.\n");
        return Object; 
    }

    Symbol res_type = No_type;
    for (size_t i = 0; i < n; i++) {
        if (!check_type_conform(dep_env, formal_types[i], method_types[i], C)) {
            notify_error(std::to_string(get_line_number()) + 
            ": In call of method " + name->get_string() + ", type " + formal_types[i]->get_string() + 
            " does not conform to declared type " + method_types[i]->get_string() + ".\n"); 
            res_type = Object;
        }
    }
    if (res_type != Object) {
        res_type = handle_SELF_TYPE(method_types.back(), init_type);
    }
    set_type(res_type);
    return res_type;
}

Symbol dispatch_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    Symbol init_type = expr->type_infer(o_env, m_env, dep_env, C);
    std::vector<Symbol> formal_types{};
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        formal_types.emplace_back(actual->nth(i)->type_infer(o_env, m_env, dep_env, C));
    } 
    Symbol cur = handle_SELF_TYPE(init_type, C);
    while (m_env.at(cur).count(name) == 0) {
        if (cur == Object) {
            notify_error(std::to_string(get_line_number()) + 
            ": Dispatch to undefined method " + name->get_string() + ".\n");
            return Object;
        }
        cur = dep_env.at(cur);
    }
    const auto& method_types = m_env.at(cur).at(name);

    size_t n = formal_types.size();
    if (n != method_types.size() - 1) {
        notify_error(std::to_string(get_line_number()) + 
        ": Method " + name->get_string() + " called with wrong number of arguments.\n");
        return Object; 
    }

    Symbol res_type = No_type;
    for (size_t i = 0; i < n; i++) {
        if (!check_type_conform(dep_env, formal_types[i], method_types[i], C)) {
            notify_error(std::to_string(get_line_number()) + 
            ": In call of method " + name->get_string() + ", type " + formal_types[i]->get_string() + 
            " does not conform to declared type " + method_types[i]->get_string() + ".\n"); 
            res_type = Object;
        }
    }
    if (res_type != Object) {
        res_type = handle_SELF_TYPE(method_types.back(), init_type);
    }
    set_type(res_type);
    return res_type;
}

Symbol cond_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    Symbol predicate = pred->type_infer(o_env, m_env, dep_env, C);
    Symbol res_type = find_least_upper_bound(dep_env, 
    then_exp->type_infer(o_env, m_env, dep_env, C), else_exp->type_infer(o_env, m_env, dep_env, C), C);
    if (predicate != Bool) {
        notify_error(std::to_string(get_line_number()) + 
        ": Predicate of 'if' have type " + predicate->get_string() + " instead Bool.\n");
        res_type = Object;
    }
    set_type(res_type);
    return res_type;
}

Symbol loop_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    Symbol predicate = pred->type_infer(o_env, m_env, dep_env, C);
    Symbol res_type = body->type_infer(o_env, m_env, dep_env, C);
    if (predicate != Bool) {
        notify_error(std::to_string(get_line_number()) + 
        ": Predicate of 'while' have type " + predicate->get_string() + " instead Bool.\n");
        res_type = Bool;
    }
    set_type(Object);
    return Object;
}

Symbol typcase_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    Symbol init_type = expr->type_infer(o_env, m_env, dep_env, C);
    // Must have some cases. This is ensured by parser.
    int i = cases->first();
    Symbol res_type = cases->nth(i)->type_infer(o_env, m_env, dep_env, C);

    std::unordered_set<Symbol> case_types{cases->nth(i)->get_type_decl()};
    for (i = cases->next(i); cases->more(i); i = cases->next(i)) {
        auto c = cases->nth(i);
        Symbol type = c->get_type_decl();
        if (case_types.find(type) != case_types.end()) {
            notify_error(std::to_string(get_line_number()) + 
            ": Duplicate branch " + type->get_string() + " in case statement.\n");
            return Object;
        }
        case_types.emplace(type);
        res_type = find_least_upper_bound(dep_env, res_type, c->type_infer(o_env, m_env, dep_env, C), C);
    }
    set_type(res_type);
    return res_type;
}

Symbol block_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    Symbol res_type;
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        res_type = body->nth(i)->type_infer(o_env, m_env, dep_env, C);
    }
    set_type(res_type);
    return res_type;
}

Symbol let_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    if (identifier == self) {
        notify_error(std::to_string(get_line_number()) + ": 'self' cannot be bound in a 'let' expression.\n"); 
        return Object;
    }

    Symbol init_type = init->type_infer(o_env, m_env, dep_env, C);
    Symbol res_type;
    // Let-No-Init
    if (init_type == No_type) {
        o_env.enterscope();
        o_env.addid(identifier, &type_decl);
        res_type = body->type_infer(o_env, m_env, dep_env, C); 
        o_env.exitscope();
        set_type(res_type);
        return res_type;
    }
    // Let-Init
    o_env.enterscope();
    o_env.addid(identifier, &type_decl);
    if (!check_type_conform(dep_env, init_type, type_decl, C)) {
        notify_error(std::to_string(get_line_number()) + ": Inferred type " + 
        init_type->get_string() + " of initialization of " + identifier->get_string() + 
        " does not conform to identifier's declared type " + type_decl->get_string() + " .\n");
        body->type_infer(o_env, m_env, dep_env, C);
        res_type = Object;
    } else {
        res_type = body->type_infer(o_env, m_env, dep_env, C); 
    }
    o_env.exitscope();
    set_type(res_type);
    return res_type;
}

Symbol plus_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    Symbol sub_type_1 = e1->type_infer(o_env, m_env, dep_env, C);
    Symbol sub_type_2 = e2->type_infer(o_env, m_env, dep_env, C);
    if (sub_type_1 != Int || sub_type_2 != Int) {
        notify_error(std::to_string(get_line_number()) + 
        ": non-Int arguments: " + sub_type_1->get_string() + " + " + sub_type_2->get_string() + ".\n");
    }
    set_type(Int);
    return Int;
}

Symbol sub_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    Symbol sub_type_1 = e1->type_infer(o_env, m_env, dep_env, C);
    Symbol sub_type_2 = e2->type_infer(o_env, m_env, dep_env, C);
    if (sub_type_1 != Int || sub_type_2 != Int) {
        notify_error(std::to_string(get_line_number()) + 
        ": non-Int arguments: " + sub_type_1->get_string() + " - " + sub_type_2->get_string() + ".\n");
    }
    set_type(Int);
    return Int;
}

Symbol mul_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    Symbol sub_type_1 = e1->type_infer(o_env, m_env, dep_env, C);
    Symbol sub_type_2 = e2->type_infer(o_env, m_env, dep_env, C);
    if (sub_type_1 != Int || sub_type_2 != Int) {
        notify_error(std::to_string(get_line_number()) + 
        ": non-Int arguments: " + sub_type_1->get_string() + " * " + sub_type_2->get_string() + ".\n");
    }
    set_type(Int);
    return Int;
}

Symbol divide_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    Symbol sub_type_1 = e1->type_infer(o_env, m_env, dep_env, C);
    Symbol sub_type_2 = e2->type_infer(o_env, m_env, dep_env, C);
    if (sub_type_1 != Int || sub_type_2 != Int) {
        notify_error(std::to_string(get_line_number()) + 
        ": non-Int arguments: " + sub_type_1->get_string() + " / " + sub_type_2->get_string() + ".\n");
    }
    set_type(Int);
    return Int;
}

Symbol neg_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    Symbol sub_type = e1->type_infer(o_env, m_env, dep_env, C);
    if (sub_type != Int) {
        notify_error(std::to_string(get_line_number()) + 
        ": Argument of '~' has type Bool instead of " + sub_type->get_string() + ".\n");
    }
    set_type(Int);
    return Int;
}

Symbol lt_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    Symbol sub_type_1 = e1->type_infer(o_env, m_env, dep_env, C);
    Symbol sub_type_2 = e2->type_infer(o_env, m_env, dep_env, C);
    if (sub_type_1 != Int || sub_type_2 != Int) {
        notify_error(std::to_string(get_line_number()) + 
        ": non-Int arguments: " + sub_type_1->get_string() + " < " + sub_type_2->get_string() + ".\n");
    }
    set_type(Bool);
    return Bool;
}

Symbol eq_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    Symbol sub_type_1 = e1->type_infer(o_env, m_env, dep_env, C);
    Symbol sub_type_2 = e2->type_infer(o_env, m_env, dep_env, C);
    if (sub_type_1 == Int || sub_type_1 == Str || sub_type_1 == Bool
    || sub_type_2 == Int || sub_type_2 == Str || sub_type_2 == Bool) {
        if (sub_type_1 != sub_type_2) {
            notify_error(std::to_string(get_line_number()) + ": Illegal comparison with a basic type.\n"); 
        }
    }
    set_type(Bool);
    return Bool;
}

Symbol leq_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    Symbol sub_type_1 = e1->type_infer(o_env, m_env, dep_env, C);
    Symbol sub_type_2 = e2->type_infer(o_env, m_env, dep_env, C);
    if (sub_type_1 != Int || sub_type_2 != Int) {
        notify_error(std::to_string(get_line_number()) + 
        ": non-Int arguments: " + sub_type_1->get_string() + " <= " + sub_type_2->get_string() + ".\n");
    }
    set_type(Bool);
    return Bool;
}

Symbol comp_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    Symbol sub_type = e1->type_infer(o_env, m_env, dep_env, C);
    if (sub_type != Bool) {
        notify_error(std::to_string(get_line_number()) + 
        ": Argument of 'not' has type Int instead of " + sub_type->get_string() + ".\n");
    }
    set_type(Bool);
    return Bool;
}

Symbol int_const_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    set_type(Int);
    return Int;
}

Symbol string_const_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    set_type(Str);
    return Str;
}

Symbol bool_const_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    set_type(Bool);
    return Bool;
}

Symbol new__class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    if (type_name != SELF_TYPE && dep_env.find(type_name) == dep_env.end()) {
        notify_error(std::to_string(get_line_number()) + 
        ": 'new' used with undefined class " + type_name->get_string() + ".\n"); 
    }
    set_type(type_name);
    return type_name;
}

Symbol isvoid_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    e1->type_infer(o_env, m_env, dep_env, C);
    set_type(Bool);
    return Bool;
}

Symbol no_expr_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    set_type(No_type);
    return No_type;
}

Symbol object_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    Symbol origin_type = Object;
    if (o_env.lookup(name) == NULL) {
        notify_error(std::to_string(get_line_number()) + 
        ": Undeclared variable " + name->get_string() + ".\n");
    } else {
        origin_type = *o_env.lookup(name);
    }
    set_type(origin_type);
    return origin_type;
}

////////////////////////////////////////////////////////////////////
//
//     Case class 
//
////////////////////////////////////////////////////////////////////

Symbol branch_class::type_infer(O_Env o_env, MethodEnv m_env, DepEnv dep_env, Symbol C) {
    o_env.enterscope();
    o_env.addid(name, &type_decl);
    Symbol res_type = expr->type_infer(o_env, m_env, dep_env, C);
    o_env.exitscope();
    return res_type;
}


////////////////////////////////////////////////////////////////////
//
//     Helper method 
//
////////////////////////////////////////////////////////////////////

// Maybe it's the user's responsibility to ensure that types are not No_type.
// Check whether type_1 < type_2 (type_1 is the subclass of type_2)
static bool check_type_conform(DepEnv dep_env, Symbol type_1, Symbol type_2, Symbol C) {
    if (type_2 == SELF_TYPE) {
        return type_1 == SELF_TYPE;
    }
    if (type_1 == SELF_TYPE) {
        type_1 = C;
    }
    Symbol cur = type_1;
    if (type_2 == Object) {
        return true;
    }
    while (cur != Object) {
        if (cur == type_2) {
            return true;
        }
        cur = dep_env.at(cur);
    }
    return false;
}

// Find the smallest commmon ancestor of type 1 and type 2
static Symbol find_least_upper_bound(DepEnv dep_env, Symbol type_1, Symbol type_2, Symbol C) {
    if (type_1 == SELF_TYPE) {
        type_1 = C;
    }
    if (type_2 == SELF_TYPE) {
        type_2 = C;
    }
    Symbol cur = type_1;
    std::unordered_set<Symbol> parents{cur};
    // Don't use do... while... because of `at()`
    while (cur != Object) {
        cur = dep_env.at(cur);
        parents.emplace(cur);
    }
    cur = type_2;
    while (parents.find(cur) == parents.end()) {
        cur = dep_env.at(cur);
    }
    return cur;
}

static Symbol handle_SELF_TYPE(Symbol type, Symbol C) {
    if (type == SELF_TYPE) {
        return C;
    }
    return type;
}
