
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream &str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol arg, arg2, Bool, concat, cool_abort, copy, Int, in_int, in_string, IO, length, Main, main_meth,
    No_class, No_type, Object, out_int, out_string, prim_slot, self, SELF_TYPE, Str, str_field, substr,
    type_name, val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void) {
  arg = idtable.add_string("arg");
  arg2 = idtable.add_string("arg2");
  Bool = idtable.add_string("Bool");
  concat = idtable.add_string("concat");
  cool_abort = idtable.add_string("abort");
  copy = idtable.add_string("copy");
  Int = idtable.add_string("Int");
  in_int = idtable.add_string("in_int");
  in_string = idtable.add_string("in_string");
  IO = idtable.add_string("IO");
  length = idtable.add_string("length");
  Main = idtable.add_string("Main");
  main_meth = idtable.add_string("main");
  //   _no_class is a symbol that can't be the name of any
  //   user-defined class.
  No_class = idtable.add_string("_no_class");
  No_type = idtable.add_string("_no_type");
  Object = idtable.add_string("Object");
  out_int = idtable.add_string("out_int");
  out_string = idtable.add_string("out_string");
  prim_slot = idtable.add_string("_prim_slot");
  self = idtable.add_string("self");
  SELF_TYPE = idtable.add_string("SELF_TYPE");
  Str = idtable.add_string("String");
  str_field = idtable.add_string("_str_field");
  substr = idtable.add_string("substr");
  type_name = idtable.add_string("type_name");
  val = idtable.add_string("_val");
}

static char *gc_init_names[] = {"_NoGC_Init", "_GenGC_Init", "_ScnGC_Init"};
static char *gc_collect_names[] = {"_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect"};

//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) {
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes, os);

  os << "\n# end of generated code\n";
}

//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream &s) {
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream &s) {
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")" << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream &s) { s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream &s) {
  s << LA << dest_reg << " " << address << endl;
}

static void emit_partial_load_address(char *dest_reg, ostream &s) { s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst &b, ostream &s) {
  emit_partial_load_address(dest, s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream &s) {
  emit_partial_load_address(dest, s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream &s) {
  emit_partial_load_address(dest, s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream &s) {
  s << MOVE << dest_reg << " " << source_reg << endl;
}

static void emit_neg(char *dest, char *src1, ostream &s) { s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream &s) {
  s << ADD << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addu(char *dest, char *src1, char *src2, ostream &s) {
  s << ADDU << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addiu(char *dest, char *src1, int imm, ostream &s) {
  s << ADDIU << dest << " " << src1 << " " << imm << endl;
}

static void emit_div(char *dest, char *src1, char *src2, ostream &s) {
  s << DIV << dest << " " << src1 << " " << src2 << endl;
}

static void emit_mul(char *dest, char *src1, char *src2, ostream &s) {
  s << MUL << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sub(char *dest, char *src1, char *src2, ostream &s) {
  s << SUB << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sll(char *dest, char *src1, int num, ostream &s) {
  s << SLL << dest << " " << src1 << " " << num << endl;
}

static void emit_jalr(char *dest, ostream &s) { s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address, ostream &s) { s << JAL << address << endl; }

static void emit_return(ostream &s) { s << RET << endl; }

static void emit_gc_assign(ostream &s) { s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream &s) { s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream &s) { s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s) { s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream &s) { s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream &s) {
  s << classname << METHOD_SEP << methodname;
}

static void emit_label_def(int l, ostream &s) {
  emit_label_ref(l, s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s) {
  s << BEQZ << source << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s) {
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s) {
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s) {
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s) {
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s) {
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s) {
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_branch(int l, ostream &s) {
  s << BRANCH;
  emit_label_ref(l, s);
  s << endl;
}

// Push a register on the stack. The stack grows towards smaller addresses.
static void emit_push(char *reg, ostream &str) {
  emit_store(reg, 0, SP, str);
  emit_addiu(SP, SP, -4, str);
}

static void emit_get_top(char *reg, ostream &str) { emit_load(reg, 1, SP, str); }

// addiu sp, sp, size * WORD_SIZE
static void emit_pop(int size, ostream &str) { emit_addiu(SP, SP, size * WORD_SIZE, str); }

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream &s) {
  emit_load(dest, DEFAULT_OBJFIELDS, source, s);
}

static void emit_fetch_bool(char *dest, char *source, ostream &s) {
  emit_load(dest, DEFAULT_OBJFIELDS, source, s);
}

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream &s) {
  emit_store(source, DEFAULT_OBJFIELDS, dest, s);
}

static void emit_store_bool(char *source, char *dest, ostream &s) {
  emit_store(source, DEFAULT_OBJFIELDS, dest, s);
}

static void emit_test_collector(ostream &s) {
  emit_push(ACC, s);
  emit_move(ACC, SP, s);  // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP, SP, 4, s);
  emit_load(ACC, 0, SP, s);
}

static void emit_gc_check(char *source, ostream &s) {
  if (source != (char *)A1)
    emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

//
// Emits code to create a new frame
// size is the offset.
//
static void emit_start_frame(int size, ostream &s) {
  emit_addiu(SP, SP, -size * WORD_SIZE, s);
  emit_store(FP, size--, SP, s);
  emit_store(SELF, size--, SP, s);
  emit_store(RA, size, SP, s);
}

// We load FP... from size, but we need to recycle allsize stack space.
static void emit_end_frame(int all_size, int size, ostream &s) {
  emit_load(FP, size, SP, s);
  emit_load(SELF, size - 1, SP, s);
  emit_load(RA, size - 2, SP, s);
  emit_addiu(SP, SP, all_size * WORD_SIZE, s);
  emit_return(s);
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream &s) { s << STRCONST_PREFIX << index; }

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream &s, int stringclasstag) {
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);
  s << LABEL                                                              // label
    << WORD << stringclasstag << endl                                     // tag
    << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len + 4) / 4) << endl // size
    << WORD << Str << DISPTAB_SUFFIX << endl                              // dispatch table
    << WORD;
  lensym->code_ref(s);
  s << endl;                    // string length
  emit_string_constant(s, str); // ascii string
  s << ALIGN;                   // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream &s, int stringclasstag) {
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s, stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s) { s << INTCONST_PREFIX << index; }

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag) {
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);
  s << LABEL                                           // label
    << WORD << intclasstag << endl                     // class tag
    << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl // object size
    << WORD << Int << DISPTAB_SUFFIX << endl;          // dispatch table
  s << WORD << str << endl;                            // integer value
}

//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag) {
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s, intclasstag);
}

//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream &s) const { s << BOOLCONST_PREFIX << val; }

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream &s, int boolclasstag) {
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);
  s << LABEL                                            // label
    << WORD << boolclasstag << endl                     // class tag
    << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl // object size
    << WORD << Bool << DISPTAB_SUFFIX << endl;          // dispatch table
  s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data() {
  Symbol main = idtable.lookup_string(MAINNAME);
  Symbol string = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL;
  emit_protobj_ref(main, str);
  str << endl;
  str << GLOBAL;
  emit_protobj_ref(integer, str);
  str << endl;
  str << GLOBAL;
  emit_protobj_ref(string, str);
  str << endl;
  str << GLOBAL;
  falsebool.code_ref(str);
  str << endl;
  str << GLOBAL;
  truebool.code_ref(str);
  str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL << WORD << stringclasstag << endl;
}

//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text() {
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"), str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag) {
  falsebool.code_def(str, boolclasstag);
  truebool.code_def(str, boolclasstag);
}

void CgenClassTable::code_select_gc() {
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}

//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants() {
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str, stringclasstag);
  inttable.code_string_table(str, intclasstag);
  code_bools(boolclasstag);
}

CgenClassTable::CgenClassTable(Classes classes, ostream &s) : nds(NULL), str(s) {
  stringclasstag = 4 /* Change to your String class tag here */;
  intclasstag = 2 /* Change to your Int class tag here */;
  boolclasstag = 3 /* Change to your Bool class tag here */;

  enterscope();
  if (cgen_debug)
    cout << "Building CgenClassTable" << endl;
  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();

  code();
  exitscope();
}

void CgenClassTable::install_basic_classes() {

  // The tree package uses these globals to annotate the classes built below.
  // curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

  //
  // A few special class names are installed in the lookup table but not
  // the class list.  Thus, these classes exist, but are not part of the
  // inheritance hierarchy.
  // No_class serves as the parent of Object and the other special classes.
  // SELF_TYPE is the self class; it cannot be redefined or inherited.
  // prim_slot is a class known to the code generator.
  //
  addid(No_class,
        new CgenNode(class_(No_class, No_class, nil_Features(), filename), Basic, this, INVALID_TAG));
  addid(SELF_TYPE,
        new CgenNode(class_(SELF_TYPE, No_class, nil_Features(), filename), Basic, this, INVALID_TAG));
  addid(prim_slot,
        new CgenNode(class_(prim_slot, No_class, nil_Features(), filename), Basic, this, INVALID_TAG));

  //
  // The Object class has no parent class. Its methods are
  //        cool_abort() : Object    aborts the program
  //        type_name() : Str        returns a string representation of class
  //        name copy() : SELF_TYPE       returns a copy of the object
  //
  // There is no need for method bodies in the basic classes---these
  // are already built in to the runtime system.
  //
  install_class(new CgenNode(
      class_(Object, No_class,
             append_Features(
                 append_Features(single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                                 single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                 single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
             filename),
      Basic, this, INVALID_TAG));

  //
  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE          writes a string to the output
  //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
  //        in_string() : Str                    reads a string from the input
  //        in_int() : Int                         "   an int     "  "     "
  //
  install_class(new CgenNode(
      class_(IO, Object,
             append_Features(
                 append_Features(
                     append_Features(single_Features(method(out_string, single_Formals(formal(arg, Str)),
                                                            SELF_TYPE, no_expr())),
                                     single_Features(method(out_int, single_Formals(formal(arg, Int)),
                                                            SELF_TYPE, no_expr()))),
                     single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                 single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
             filename),
      Basic, this, INVALID_TAG));

  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer.
  //
  install_class(new CgenNode(class_(Int, Object, single_Features(attr(val, prim_slot, no_expr())), filename),
                             Basic, this, INVALID_TAG));

  //
  // Bool also has only the "val" slot.
  //
  install_class(new CgenNode(class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename),
                             Basic, this, INVALID_TAG));

  //
  // The class Str has a number of slots and operations:
  //       val                                  ???
  //       str_field                            the string itself
  //       length() : Int                       length of the string
  //       concat(arg: Str) : Str               string concatenation
  //       substr(arg: Int, arg2: Int): Str     substring
  //
  install_class(new CgenNode(
      class_(
          Str, Object,
          append_Features(
              append_Features(
                  append_Features(append_Features(single_Features(attr(val, Int, no_expr())),
                                                  single_Features(attr(str_field, prim_slot, no_expr()))),
                                  single_Features(method(length, nil_Formals(), Int, no_expr()))),
                  single_Features(method(concat, single_Formals(formal(arg, Str)), Str, no_expr()))),
              single_Features(method(
                  substr, append_Formals(single_Formals(formal(arg, Int)), single_Formals(formal(arg2, Int))),
                  Str, no_expr()))),
          filename),
      Basic, this, INVALID_TAG));
}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd) {
  Symbol name = nd->get_name();

  if (probe(name)) {
    return;
  }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd, nds);
  addid(name, nd);
}

void CgenClassTable::install_classes(Classes cs) {
  for (int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i), NotBasic, this, INVALID_TAG));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree() {
  for (List<CgenNode> *l = nds; l; l = l->tl())
    set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd) {
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n) { children = new List<CgenNode>(n, children); }

void CgenNode::set_parentnd(CgenNodeP p) {
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

//******************************************************************
//
//   Set the index of all classes
//
//*****************************************************************

void CgenClassTable::set_class_index(CgenNodeP cur) {
  if (cur->get_name() == Int) {
    intclasstag = tag_index;
  } else if (cur->get_name() == Str) {
    stringclasstag = tag_index;
  } else if (cur->get_name() == Bool) {
    boolclasstag = tag_index;
  }
  cur->set_tag(tag_index++);
  auto children_list = cur->get_children();
  for (List<CgenNode> *l = children_list; l; l = l->tl()) {
    auto *node = l->hd();
    set_class_index(node);
  }
}

//******************************************************************
//
//   Emit the prototype of all classes
//
//*****************************************************************

void CgenClassTable::code_prototype() {
  for (List<CgenNode> *l = nds; l; l = l->tl()) {
    auto *node = l->hd();
    auto *name = node->get_name()->get_string();
    node->cal_proto_size();
    str << WORD << -1 << endl
        << name << PROTOBJ_SUFFIX << LABEL << WORD << node->get_tag() << endl // tag
        << WORD << node->get_proto_size() << endl                             // size
        << WORD << name << DISPTAB_SUFFIX << endl;                            // dispatch table
    node->emit_default_attrs(str);
  }
}

//******************************************************************
//
//   Emit the nameTab and the objTab of all classes
//
//*****************************************************************

void CgenClassTable::code_class_nameTab() {
  str << CLASSNAMETAB << LABEL;
  code_class_nameTab(root());
}

void CgenClassTable::code_class_nameTab(CgenNodeP cur) {
  auto *entry = stringtable.lookup_string(cur->get_name()->get_string());
  str << WORD;
  entry->code_ref(str);
  str << endl;

  auto children_list = cur->get_children();
  for (List<CgenNode> *l = children_list; l; l = l->tl()) {
    auto *node = l->hd();
    code_class_nameTab(node);
  }
}

void CgenClassTable::code_class_objTab() {
  str << CLASSOBJTAB << LABEL;
  code_class_objTab(root());
}

void CgenClassTable::code_class_objTab(CgenNodeP cur) {
  Symbol name = cur->get_name();
  str << WORD << name << PROTOBJ_SUFFIX << endl;
  str << WORD << name << CLASSINIT_SUFFIX << endl;

  auto children_list = cur->get_children();
  for (List<CgenNode> *l = children_list; l; l = l->tl()) {
    auto *node = l->hd();
    code_class_objTab(node);
  }
}

//******************************************************************
//
//   Emit the dispatchTab of all classes
//
//*****************************************************************

// Use bfs because i don't want to add another func for dfs
void CgenClassTable::code_dispatchTab() {
  auto cur = root();
  std::deque<CgenNodeP> bfs{cur};
  while (!bfs.empty()) {
    cur = bfs.front();
    str << cur->get_name() << DISPTAB_SUFFIX << LABEL;
    cur->emit_methods(str);
    bfs.pop_front();
    for (List<CgenNode> *l = cur->get_children(); l; l = l->tl()) {
      auto node = l->hd();
      bfs.emplace_back(node);
    }
  }
}

//******************************************************************
//
//   Emit the initializer of all classes
//
//*****************************************************************

void CgenClassTable::code_initializer(MEnv m_pos) {
  for (List<CgenNode> *l = nds; l; l = l->tl()) {
    auto *node = l->hd();
    node->emit_init(m_pos, str);
  }
}

//******************************************************************
//
//   Emit the methods of all classes
//
//*****************************************************************

void CgenClassTable::code_methods(MEnv m_pos) {
  for (List<CgenNode> *l = nds; l; l = l->tl()) {
    auto *node = l->hd();
    auto node_name = node->get_name();
    // The methods are defined in the runtime system.
    if (node_name == Object || node_name == Str || node_name == IO) {
      continue;
    }
    node->emit_method_def(m_pos, str);
  }
}

void CgenClassTable::code() {
  set_class_index(root());

  if (cgen_debug) {
    cout << "coding global data" << endl;
  }
  code_global_data();

  if (cgen_debug) {
    cout << "choosing gc" << endl;
  }
  code_select_gc();

  if (cgen_debug) {
    cout << "coding constants" << endl;
  }
  code_constants();

  if (cgen_debug) {
    cout << "coding prototype objects" << endl;
  }
  code_prototype();

  if (cgen_debug) {
    cout << "coding objTab, nameTab, dispatchTab" << endl;
  }

  code_class_objTab();

  code_class_nameTab();

  code_dispatchTab();

  if (cgen_debug) {
    cout << "coding global text" << endl;
  }
  code_global_text();

  // Collect the pos for the initializer and class methods
  collect_pos();

  if (cgen_debug) {
    cout << "coding object initializer" << endl;
  }
  code_initializer(class_m_pos);

  if (cgen_debug) {
    cout << "coding object methods" << endl;
  }
  code_methods(class_m_pos);
}

CgenNodeP CgenClassTable::root() { return probe(Object); }

// Ensure that the parent class is initialized before the subclass.
void CgenClassTable::collect_pos() {
  auto cur = root();
  std::deque<CgenNodeP> bfs{cur};
  while (!bfs.empty()) {
    cur = bfs.front();
    cur->collect_pos();
    class_m_pos.emplace(cur->get_name(), cur->get_methods_pos());
    bfs.pop_front();
    for (List<CgenNode> *l = cur->get_children(); l; l = l->tl()) {
      auto node = l->hd();
      bfs.emplace_back(node);
    }
  }
}

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct, int tag)
    : class__class((const class__class &)*nd), parentnd(NULL), children(NULL), basic_status(bstatus),
      class_tag(tag) {
  stringtable.add_string(name->get_string()); // Add class name to string table
  if (tag != -1) {
    collect_info();
    cgen_tab = ct;
  }
}

int CgenNode::get_tag() const {
  if (cgen_debug) {
    assert(class_tag != -1);
  }
  return class_tag;
}

void CgenNode::emit_methods(ostream &str) {
  method_layout = parentnd->inherit_method_layout();
  m_pos = parentnd->inherit_methods_pos();

  for (auto [m_name, method] : methods) {
    if (m_pos.find(m_name) != m_pos.end()) {
      method_layout[m_pos[m_name]].second = name;
    } else {
      m_pos.emplace(m_name, m_pos.size());
      method_layout.emplace_back(m_name, name);
    }
  }

  for (auto [m_name, c_name] : method_layout) {
    str << WORD;
    emit_method_ref(c_name, m_name, str);
    str << endl;
  }

  if (cgen_debug) {
    cout << name << " METHODS: " << endl;
    for (auto [m_name, c_name] : method_layout) {
      cout << '\t' << c_name << " " << m_name << " at offset " << m_pos[m_name] << endl;
    }
  }
}

void CgenNode::emit_default_attrs(ostream &str) {
  if (parentnd->get_name() != No_class) {
    parentnd->emit_default_attrs(str);
  }
  for (auto &attr : attrs) {
    // Symbol is a pointer so use value semantic.
    auto a_type = attr.second->collect_type();
    str << WORD;
    if (a_type == Int) {
      inttable.lookup_string("0")->code_ref(str);
    } else if (a_type == Bool) {
      falsebool.code_ref(str);
    } else if (a_type == Str) {
      stringtable.lookup_string("")->code_ref(str);
    } else {
      str << 0;
    }
    str << endl;
  }
}

void CgenNode::cal_proto_size() {
  proto_size = DEFAULT_OBJFIELDS + get_attrs_size();
  CgenNodeP cur = parentnd;
  while (cur->get_name() != No_class) {
    proto_size += cur->get_attrs_size();
    cur = cur->get_parentnd();
  }
}

void CgenNode::emit_init(MEnv m_env, ostream &str) {
  auto sym_tab = new SymbolTable<Symbol, int>();

  str << name << CLASSINIT_SUFFIX << LABEL;

  int num_temp = 0;
  for (auto [a_name, attr] : attrs) {
    num_temp = max(num_temp, attr->get_num_temp());
  }
  emit_start_frame(DEFAULT_OBJFIELDS + num_temp, str);
  emit_addiu(FP, SP, 4, str);
  emit_move(SELF, ACC, str);
  if (name != Object) {
    std::string ra_place = std::string(parentnd->get_name()->get_string()) + CLASSINIT_SUFFIX;
    emit_jal(&ra_place[0], str);
  }
  int attr_start_index = proto_size - attrs.size();
  for (auto [a_name, attr] : attrs) {
    if (attr->has_init_expr()) {
      attr->code(sym_tab, 0, this, m_env, str);
      emit_store(ACC, attr_start_index, SELF, str);
    }
    attr_start_index++;
  }
  emit_move(ACC, SELF, str);
  emit_end_frame(DEFAULT_OBJFIELDS + num_temp, DEFAULT_OBJFIELDS + num_temp, str);

  delete sym_tab;
}

void CgenNode::collect_pos() {
  CgenNodeP cur = parentnd;
  attrs_pos = cur->inherit_attrs_pos();

  int attr_start_index = proto_size - attrs.size();
  for (auto [a_name, attr] : attrs) {
    attrs_pos.emplace(a_name, attr_start_index++);
  }

  if (cgen_debug) {
    cout << name << " ATTR: " << endl;
    for (auto [a_name, a_pos] : attrs_pos) {
      cout << '\t' << name << " " << a_name << " " << a_pos << endl;
    }
  }
}

void CgenNode::emit_method_def(MEnv m_env, ostream &str) {
  auto sym_tab = new SymbolTable<Symbol, int>();
  for (auto [m_name, method] : methods) {
    emit_method_ref(name, m_name, str);
    str << ':' << endl;
    method->code(sym_tab, 0, this, m_env, str);
  }
  delete sym_tab;
}

int CgenNode::get_sub_maxtag() {
  int res = class_tag;
  if (children == NULL) {
    return res;
  }
  List<CgenNode> *l = children;
  while (l->tl()) {
    l = l->tl();
  }
  return l->hd()->get_sub_maxtag();
}

///////////////////////////////////////////////////////////////////////
//
//  Calculating the temporary variables of methods.
//
///////////////////////////////////////////////////////////////////////

void method_class::cal_num_temp() { num_temp = expr->cal_num_temp(); }

void attr_class::cal_num_temp() { num_temp = init->cal_num_temp(); }

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void method_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  o_pos->enterscope();
  // Record the formal pos
  int n = formals->len();
  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    auto f = formals->nth(i);
    o_pos->addid(f->get_name(), new int(n - i - 1 + 3 + num_temp));
  }
  emit_start_frame(DEFAULT_OBJFIELDS + num_temp, s);
  emit_addiu(FP, SP, 4, s);
  emit_move(SELF, ACC, s);
  expr->code(o_pos, temp_index, node, m_pos, s);
  emit_end_frame(DEFAULT_OBJFIELDS + num_temp + formals->len(), DEFAULT_OBJFIELDS + num_temp, s);
  o_pos->exitscope();
}

void attr_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  o_pos->enterscope();
  init->code(o_pos, temp_index, node, m_pos, s);
  o_pos->exitscope();
}

void assign_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  expr->code(o_pos, temp_index, node, m_pos, s);
  auto pos = o_pos->lookup(name);
  if (pos != NULL) {
    emit_store(ACC, *pos, FP, s);
  } else {
    emit_store(ACC, node->get_attrs_pos().at(name), SELF, s);
  }
}

// This is copied from dispatch_class
void static_dispatch_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  int no_void_obj = label_index++;

  int n = actual->len();
  emit_addiu(SP, SP, -n * WORD_SIZE, s);
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(o_pos, temp_index, node, m_pos, s);
    emit_store(ACC, n - i, SP, s);
  }

  expr->code(o_pos, temp_index, node, m_pos, s);
  emit_bne(ACC, ZERO, no_void_obj, s);

  emit_partial_load_address(ACC, s);
  stringtable.lookup_string(node->get_filename()->get_string())->code_ref(s);
  s << endl;
  emit_load_imm(T1, get_line_number(), s);
  s << JAL << " _dispatch_abort" << endl;

  emit_label_def(no_void_obj, s);
  // load dispatch table
  emit_partial_load_address(T1, s);
  s << type_name << DISPTAB_SUFFIX << endl;
  int m_index = m_pos.at(type_name).at(name);
  emit_load(T1, m_index, T1, s);
  emit_jalr(T1, s);
}

void dispatch_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  int no_void_obj = label_index++;

  // All subexprs must be evaluated before e0.
  int n = actual->len();
  if (n != 0) {
    emit_addiu(SP, SP, -n * WORD_SIZE, s);
  }
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(o_pos, temp_index, node, m_pos, s);
    emit_store(ACC, n - i, SP, s);
  }

  expr->code(o_pos, temp_index, node, m_pos, s);
  emit_bne(ACC, ZERO, no_void_obj, s);

  emit_partial_load_address(ACC, s);
  stringtable.lookup_string(node->get_filename()->get_string())->code_ref(s);
  s << endl;
  emit_load_imm(T1, get_line_number(), s);
  s << JAL << " _dispatch_abort" << endl;

  emit_label_def(no_void_obj, s);
  // load dispatch pointer
  emit_load(T1, 2, ACC, s);
  int m_index = m_pos.at(handle_SELF_TYPE(expr->get_type(), node->get_name())).at(name);
  emit_load(T1, m_index, T1, s);
  emit_jalr(T1, s);
}

void cond_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  int false_br = label_index++;
  int true_br = label_index++;
  int end_if = label_index++;
  pred->code(o_pos, temp_index, node, m_pos, s);
  // load bool val
  emit_fetch_bool(T1, ACC, s);
  emit_beqz(T1, false_br, s);
  emit_label_def(true_br, s);
  then_exp->code(o_pos, temp_index, node, m_pos, s);
  emit_branch(end_if, s);
  emit_label_def(false_br, s);
  else_exp->code(o_pos, temp_index, node, m_pos, s);
  emit_label_def(end_if, s);
}

void loop_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  int begin_loop = label_index++;
  int end_loop = label_index++;
  emit_label_def(begin_loop, s);
  pred->code(o_pos, temp_index, node, m_pos, s);
  // load bool val
  emit_fetch_bool(T1, ACC, s);
  emit_beqz(T1, end_loop, s);
  body->code(o_pos, temp_index, node, m_pos, s);
  s << BRANCH;
  emit_label_ref(begin_loop, s);
  s << endl;
  emit_label_def(end_loop, s);
  emit_move(ACC, ZERO, s);
}

void typcase_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  // Tag of class itself, pointer of branch
  std::vector<std::pair<CgenNodeP, branch_class *>> branches{};

  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    auto branch = (branch_class *)cases->nth(i);
    CgenNodeP cur = node->get_cgen_tab()->probe(branch->type_decl);
    branches.emplace_back(cur, branch);
  }

  sort(branches.begin(), branches.end(),
       [](const auto a, const auto b) { return a.first->get_tag() > b.first->get_tag(); });

  if (cgen_debug) {
    cout << "A CASE STATEMENT: " << endl;
    for (auto [x, y] : branches) {
      cout << '\t' << y->name << ":\t" << x->get_tag() << " " << x->get_sub_maxtag() << endl;
    }
  }
  int end_case = label_index++;
  int begin_case = label_index++;

  expr->code(o_pos, temp_index, node, m_pos, s);
  emit_bne(ACC, ZERO, begin_case, s);
  emit_partial_load_address(ACC, s);
  stringtable.lookup_string(node->get_filename()->get_string())->code_ref(s);
  s << endl;
  emit_load_imm(T1, get_line_number(), s);
  s << JAL << " _case_abort2" << endl;

  emit_label_def(begin_case, s);
  emit_store(ACC, temp_index, FP, s);
  // load tag
  emit_load(T1, 0, ACC, s);
  // Preserve T1
  emit_push(T1, s);

  int n = branches.size();
  for (int i = 0; i < n; i++) {
    auto c = branches[i].first;
    int c_tag = c->get_tag();
    int sub_max_tag = c->get_sub_maxtag();
    int b = label_index++;

    emit_blti(T1, c_tag, b, s);
    emit_bgti(T1, sub_max_tag, b, s);

    o_pos->enterscope();
    o_pos->addid(branches[i].second->name, new int(temp_index));
    branches[i].second->expr->code(o_pos, temp_index + 1, node, m_pos, s);
    o_pos->exitscope();

    emit_branch(end_case, s);
    emit_label_def(b, s);
    emit_get_top(T1, s);
  }
  s << JAL << " _case_abort" << endl;
  emit_label_def(end_case, s);
  emit_pop(1, s);
}

void block_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    body->nth(i)->code(o_pos, temp_index, node, m_pos, s);
  }
}

void let_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  if (dynamic_cast<no_expr_class *>(init) != nullptr) {
    emit_default(type_decl, s);
  } else {
    init->code(o_pos, temp_index + 1, node, m_pos, s);
  }
  o_pos->enterscope();
  o_pos->addid(identifier, new int(temp_index));
  emit_store(ACC, temp_index, FP, s);
  body->code(o_pos, temp_index + 1, node, m_pos, s);
  o_pos->exitscope();
}

// sub, mul, div are copied from here.
void plus_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  e1->code(o_pos, temp_index, node, m_pos, s);
  emit_push(ACC, s);
  e2->code(o_pos, temp_index, node, m_pos, s);

  // Copy a new int Object
  s << JAL;
  emit_method_ref(Object, idtable.add_string("copy"), s);
  s << endl;

  // After copy
  emit_get_top(T1, s);

  // Int(i1) in T1, Int(i2) in ACC
  // i1 in T1, i2 in T2
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);
  emit_add(T1, T2, T1, s);
  // get i1 + i2, then store Int(i1 + i2) into ACC
  emit_store_int(T1, ACC, s);
  emit_pop(1, s);
}

void sub_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  e1->code(o_pos, temp_index, node, m_pos, s);
  emit_push(ACC, s);
  e2->code(o_pos, temp_index, node, m_pos, s);
  s << JAL;
  emit_method_ref(Object, idtable.add_string("copy"), s);
  s << endl;
  emit_get_top(T1, s);
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);
  emit_sub(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
  emit_pop(1, s);
}

void mul_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  e1->code(o_pos, temp_index, node, m_pos, s);
  emit_push(ACC, s);
  e2->code(o_pos, temp_index, node, m_pos, s);
  s << JAL;
  emit_method_ref(Object, idtable.add_string("copy"), s);
  s << endl;
  emit_get_top(T1, s);
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);
  emit_mul(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
  emit_pop(1, s);
}

void divide_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  e1->code(o_pos, temp_index, node, m_pos, s);
  emit_push(ACC, s);
  e2->code(o_pos, temp_index, node, m_pos, s);
  s << JAL;
  emit_method_ref(Object, idtable.add_string("copy"), s);
  s << endl;
  emit_get_top(T1, s);
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);
  emit_div(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
  emit_pop(1, s);
}

void neg_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  e1->code(o_pos, temp_index, node, m_pos, s);
  s << JAL;
  emit_method_ref(Object, idtable.add_string("copy"), s);
  s << endl;
  emit_fetch_int(T1, ACC, s);
  emit_neg(T1, T1, s);
  emit_store_int(T1, ACC, s);
}

// leq is copied from here.
void lt_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  int true_br = label_index++;
  e1->code(o_pos, temp_index, node, m_pos, s);
  emit_push(ACC, s);
  e2->code(o_pos, temp_index, node, m_pos, s);
  emit_get_top(T1, s);
  // Int(i1) in T1, Int(i2) in ACC
  // i1 in T1, i2 in T2
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);

  emit_load_bool(ACC, truebool, s);
  emit_blt(T1, T2, true_br, s);
  // false br
  emit_load_bool(ACC, falsebool, s);
  // true br
  emit_label_def(true_br, s);
  emit_pop(1, s);
}

void eq_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  int no_basic_eq = label_index++;
  e1->code(o_pos, temp_index, node, m_pos, s);
  emit_push(ACC, s);
  e2->code(o_pos, temp_index, node, m_pos, s);
  emit_get_top(T1, s);
  emit_move(T2, ACC, s);
  // Pointer semantic: T1 == T2
  emit_load_bool(ACC, truebool, s);
  emit_beq(T1, T2, no_basic_eq, s);
  emit_load_bool(A1, falsebool, s);
  // Test basic class
  emit_jal("equality_test", s);
  emit_label_def(no_basic_eq, s);
  emit_pop(1, s);
}

void leq_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  int true_br = label_index++;
  e1->code(o_pos, temp_index, node, m_pos, s);
  emit_push(ACC, s);
  e2->code(o_pos, temp_index, node, m_pos, s);
  emit_get_top(T1, s);
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);
  emit_load_bool(ACC, truebool, s);
  emit_bleq(T1, T2, true_br, s);
  emit_load_bool(ACC, falsebool, s);
  emit_label_def(true_br, s);
  emit_pop(1, s);
}

// not
void comp_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  int true_br = label_index++;
  e1->code(o_pos, temp_index, node, m_pos, s);
  emit_fetch_bool(T1, ACC, s);
  // T1 is 1 or 0.
  emit_load_bool(ACC, truebool, s);
  emit_beqz(T1, true_br, s);
  emit_load_bool(ACC, falsebool, s);
  // ACC = 1, so return false.
  emit_label_def(true_br, s);
}

void int_const_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
}

void string_const_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
}

void bool_const_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  if (type_name == SELF_TYPE) {
    //
    emit_load_address(T1, CLASSOBJTAB, s);
    emit_load(T2, 0, SELF, s);
    emit_sll(T2, T2, 3, s);
    emit_add(T1, T1, T2, s);
    // Store the T1
    emit_push(T1, s);
    emit_load(ACC, 0, T1, s);
  } else {
    // Jump to the protoOb
    emit_partial_load_address(ACC, s);
    emit_protobj_ref(type_name, s);
    s << endl;
  }
  // Copy a new  Object
  s << JAL;
  emit_method_ref(Object, idtable.add_string("copy"), s);
  s << endl;
  if (type_name == SELF_TYPE) {
    emit_get_top(T2, s);
    emit_pop(1, s);
    emit_load(T1, 1, T2, s);
    emit_jalr(T1, s);
  } else {
    s << JAL;
    emit_init_ref(type_name, s);
    s << endl;
  }
}

void isvoid_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  int true_br = label_index++;
  e1->code(o_pos, temp_index, node, m_pos, s);
  emit_move(T1, ACC, s);
  emit_load_bool(ACC, truebool, s);
  emit_beqz(T1, true_br, s);
  emit_load_bool(ACC, falsebool, s);
  emit_label_def(true_br, s);
}

void no_expr_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {}

void object_class::code(SEnv o_pos, int temp_index, CgenNodeP node, MEnv m_pos, ostream &s) {
  if (name == self) {
    emit_move(ACC, SELF, s);
    return;
  }
  auto pos = o_pos->lookup(name);
  if (pos != NULL) {
    emit_load(ACC, *pos, FP, s);
  } else {
    emit_load(ACC, node->get_attrs_pos().at(name), SELF, s);
  }
}

static void emit_default(Symbol name, ostream &s) {
  if (name == Int) {
    emit_partial_load_address(ACC, s);
    inttable.lookup_string("0")->code_ref(s);
    s << endl;
  } else if (name == Str) {
    emit_partial_load_address(ACC, s);
    stringtable.lookup_string("")->code_ref(s);
    s << endl;
  } else if (name == Bool) {
    emit_partial_load_address(ACC, s);
    falsebool.code_ref(s);
    s << endl;
  } else {
    emit_move(ACC, ZERO, s);
  }
}

//******************************************************************
//
//   Transplant from PA4
//
//*****************************************************************

void CgenNode::collect_info() {
  // We have to dynamic_cast to collect the info of methods and attrs.
  // We also check the error of them.
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    auto f = features->nth(i);
    if (method_class *method = dynamic_cast<method_class *>(f); method != nullptr) {
      add_method(method);
      method->cal_num_temp();
      if (cgen_debug) {
        cout << name << " " << method->get_name() << " needs " << method->get_num_temp() << " extra word.\n";
      }
    } else {
      if (attr_class *attr = dynamic_cast<attr_class *>(f); attr != nullptr) {
        add_attr(attr);
        attr->cal_num_temp();
        if (cgen_debug) {
          cout << name << " " << attr->get_name() << " needs " << attr->get_num_temp() << " extra word.\n";
        }
      }
    }
  }
}

void CgenNode::add_method(method_class *method) {
  Symbol name = method->get_name();
  method->collect_type();
  methods.emplace_back(name, method);
}

void CgenNode::add_attr(attr_class *attr) {
  Symbol name = attr->get_name();
  // Symbol type = attr->collect_type();
  attrs.emplace_back(name, attr);
}

const std::vector<Symbol> &method_class::collect_type() {
  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    auto f = formals->nth(i);
    Symbol formal_type = f->get_type_decl();
    types.emplace_back(formal_type);
  }
  types.emplace_back(return_type);
  return types;
}

bool attr_class::has_init_expr() { return dynamic_cast<no_expr_class *>(init) == nullptr; }

static Symbol handle_SELF_TYPE(Symbol type, Symbol C) {
  if (type == SELF_TYPE) {
    return C;
  }
  return type;
}
