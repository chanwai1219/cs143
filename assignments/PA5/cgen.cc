
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

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

int cgen_comment = 1;
int labelnum = 0;
class Environment *enviroment;
CgenClassTableP codegen_classtable;

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
Symbol 
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

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


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

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  codegen_classtable = new CgenClassTable(classes,os);

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

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
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
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


 /***** Add dispatch information for class String ******/
      emit_disptable_ref(Str, s);
      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/
      emit_disptable_ref(Int, s);
      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/
      emit_disptable_ref(Bool, s);
      s << endl;                                            // dispatch table
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

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
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

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}

void CgenClassTable::code_objects()
{
  Symbol s;
  CgenNodeP node;
  std::vector<CgenNodeP> inheritance;
  std::vector<CgenNodeP> nodes = get_class_nodes();

  /* class name table */
  str << CLASSNAMETAB << LABEL;
  for (unsigned int i = 0; i < nodes.size(); i++)
  {
    node = nodes[i];
    s = node->get_name();
    str << WORD;
    stringtable.lookup_string(s->get_string())->code_ref(str);
    str << endl;
  }

  /* object table */
  str << CLASSOBJTAB << LABEL;
  for (unsigned int i = 0; i < nodes.size(); i++)
  {
    node = nodes[i];
    s = node->get_name();

    str << WORD;
    emit_protobj_ref(s, str);
    str << endl;
    str << WORD;
    emit_init_ref(s, str);
    str << endl;
  }

  for (int i = nodes.size() - 1; i >= 0; i--)
  {
    std::vector<method_class *> methods;

    node = nodes[i];

    emit_disptable_ref(node->get_name(), str);
    str << LABEL;

    std::map<Symbol, Symbol> dispatch_class_table;
    std::map<Symbol, int> dispatch_index_table;

    methods = node->get_all_methods();
    dispatch_class_table = node->get_dispatch_class_table();

    for (unsigned int k = 0; k < methods.size(); k++)
    {
      str << WORD;
      emit_method_ref(dispatch_class_table[methods[k]->name], methods[k]->name, str);
      str << endl;
    }
  }

  for (int i = nodes.size() - 1; i >= 0; i--)
  {
    int words = 0;
    std::vector<attr_class *> attrs;

    attrs = nodes[i]->get_all_attrs();

    str << WORD << "-1" << endl;
    
    str << nodes[i]->get_name() << PROTOBJ_SUFFIX << LABEL;
    str << WORD << get_class_tag(nodes[i]->get_name()) << endl; // class tag
    str << WORD << DEFAULT_OBJFIELDS + attrs.size() << endl;    // length
    str << WORD;                                                // dispatch table
    emit_disptable_ref(nodes[i]->get_name(), str);
    str << endl;

    for (unsigned int k = 0; k < attrs.size(); k++)
    {
      str << WORD;
      str << EMPTYSLOT << endl;
    }
  }
}

void CgenClassTable::code_object_initializer()
{
  Symbol s;
  
  for (List<CgenNode> *l = nds; l; l = l->tl())
  {
    List<CgenNode> *list = NULL;
    class__class *class_;
    Features features;
    Feature feature;
    method_class *method;
    Symbol parent;
    CgenNodeP node;

    node = l->hd();

    str << node->get_name() << CLASSINIT_SUFFIX << LABEL;

    emit_addiu(SP, SP, -12, str);
    emit_store(FP, 3, SP, str);
    emit_store(SELF, 2, SP, str);
    emit_store(RA, 1, SP, str);
    emit_addiu(FP, SP, 4, str);

    emit_move(SELF, ACC, str);

    parent = node->get_parentnd()->get_name();
    if (parent != No_class)
    {
      str << JAL;
      emit_init_ref(parent, str);
      str << endl;
    }

    emit_move(ACC, SELF, str);

    emit_load(FP, 3, SP, str);
    emit_load(SELF, 2, SP, str);
    emit_load(RA, 1, SP, str);
    emit_addiu(SP, SP, 12, str);
    emit_return(str);
  }
}

void CgenClassTable::code_object_method()
{
  for (List<CgenNode> *l = nds; l; l = l->tl())
  {
    List<CgenNode> *list = NULL;
    class__class *class_;
    Features features;
    Feature feature;
    Formals formals;
    formal_class *formal;
    method_class *method;
    attr_class *attr;
    Symbol parent;
    CgenNodeP node;
    int formal_number = 0;

    node = l->hd();

    if (node->basic())
      continue;

    class_ = probe(node->get_name());
    features = class_->features;
    for (int i = features->first(); features->more(i); i = features->next(i))
    {
      feature = features->nth(i);
      if (!feature->is_method())
      {
        attr = (attr_class *)feature;
        // attr->init->code(str);
      }
      else
      {
        method = (method_class *)feature;
        formals = method->formals;

        if (enviroment)
          delete enviroment;
        enviroment = new Environment(node);
        enviroment->enter_scope();

        for (int j = formals->first(); formals->more(j); j = formals->next(j))
        {
          formal = (formal_class *)formals->nth(j);
          enviroment->add_parameter(formal->name);
          formal_number++;
        }
        emit_method_ref(node->get_name(), method->name, str);
        str << LABEL;

        emit_addiu(SP, SP, -12, str);
        emit_store(FP, 3, SP, str);
        emit_store(SELF, 2, SP, str);
        emit_store(RA, 1, SP, str);
        emit_addiu(FP, SP, 4, str);
        emit_move(SELF, ACC, str);

        method->expr->code(str);

        emit_load(FP, 3, SP, str);
        emit_load(SELF, 2, SP, str);
        emit_load(RA, 1, SP, str);
        emit_addiu(SP, SP, 12 + formal_number * WORD_SIZE, str);
        emit_return(str);
      }
    }
  }
}

CgenClassTable::CgenClassTable(Classes classes, ostream &s) : nds(NULL), str(s)
{
  enterscope();
  if (cgen_debug)
    cout << "Building CgenClassTable" << endl;
  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();

  stringclasstag = get_class_tag(Str);
  intclasstag = get_class_tag(Int);
  boolclasstag = get_class_tag(Bool);

  codegen_classtable = this;
  
  code();
  exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
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
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
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
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

std::vector<method_class *> CgenNode::get_methods()
{
  if (methods.empty())
  {
    Feature feature;
    method_class *method;

    for (int i = features->first(); features->more(i); i = features->next(i))
    {
      feature = features->nth(i);
      if (feature->is_method())
      {
        method = (method_class *)feature;
        methods.push_back(method);
      }
    }
  }
  return methods;
}

std::vector<method_class *> CgenNode::get_all_methods()
{
  if (all_methods.empty())
  {
    std::vector<CgenNode *> inheritance = get_inheritance();
    for (unsigned int i = 0; i < inheritance.size(); i++)
    {
      CgenNodeP _class_node = inheritance[i];
      Symbol _class_name = _class_node->name;
      std::vector<method_class *> _methods = _class_node->get_methods();
      for (unsigned int j = 0; j < _methods.size(); j++)
      {
        method_class *_method = _methods[j];
        Symbol _method_name = _method->name;
        if (dispatch_index_table.find(_method_name) == dispatch_index_table.end())
        {
          // method need to be inserted.
          all_methods.push_back(_method);
          dispatch_index_table[_method_name] = all_methods.size() - 1;
          dispatch_class_table[_method_name] = _class_name;
        }
        else
        {
          int idx = dispatch_index_table[_method_name];
          all_methods[idx] = _method;
          dispatch_class_table[_method_name] = _class_name;
        }
      }
    }
  }
  return all_methods;
}

std::vector<attr_class *> CgenNode::get_attrs()
{
  if (attrs.empty())
  {
    Feature feature;
    attr_class *attr;
    for (unsigned int i = features->first(); features->more(i); i = features->next(i))
    {
      feature = features->nth(i);
      if (!feature->is_method())
      {
        attr = (attr_class *)feature;
        attrs.push_back(attr);
      }
    }
  }

  return attrs;
}

std::vector<attr_class *> CgenNode::get_all_attrs()
{
  if (all_attrs.empty())
  {
    std::vector<CgenNode *> inheritance = get_inheritance();
    for (unsigned int i = 0; i < inheritance.size(); i++)
    {
      CgenNode *class_node = inheritance[i];
      Features features = class_node->features;
      for (unsigned int j = features->first(); features->more(j); j = features->next(j))
      {
        Feature feature = features->nth(j);
        if (!feature->is_method())
        {
          attr_class *attr = (attr_class *)feature;
          all_attrs.push_back(attr);
          attr_index_table[attr->name] = all_attrs.size() - 1;
        }
      }
    }
  }

  return all_attrs;
}

std::vector<CgenNode *> CgenNode::get_inheritance()
{
  if (inheritance.empty())
  {
    CgenNode *class_node = this;
    while (class_node->name != No_class)
    {
      inheritance.push_back(class_node);
      class_node = class_node->get_parentnd();
    }
    std::reverse(inheritance.begin(), inheritance.end());
  }

  return inheritance;
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//

  if (cgen_debug) cout << "coding object table" << endl;
  code_objects();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...

  if (cgen_debug) cout << "coding object initializer" << endl;
  code_object_initializer();

  code_object_method();
}

CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}

std::vector<CgenNodeP> CgenClassTable::get_class_nodes()
{
  if (class_nodes.empty())
  {
    for (List<CgenNode> *l = nds; l; l = l->tl())
    {
      CgenNodeP class_node = l->hd();
      class_nodes.push_back(class_node);
    }
    std::reverse(class_nodes.begin(), class_nodes.end());
    for (unsigned int i = 0; i < class_nodes.size(); ++i)
    {
      class_nodes[i]->set_class_tag(i);
      class_tags.insert(std::make_pair(class_nodes[i]->get_name(), i));
    }
  }
  return class_nodes;
}

std::map<Symbol, int> CgenClassTable::get_class_tags()
{
  get_class_nodes();
  return class_tags;
}

CgenNode *CgenClassTable::get_class_node(Symbol class_name)
{
  get_class_nodes();
  return class_nodes[class_tags[class_name]];
}

int CgenClassTable::get_class_tag(Symbol s)
{
  std::map<Symbol, int>::iterator iter;
  std::map<Symbol, int> tags;
  tags = get_class_tags();
  iter = tags.find(s);
  if (iter == tags.end())
  {
    return -1;
  }
  return iter->second;
}

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s)
{
  int index;

  if (cgen_comment)
    s << "\t# Assign. First eval the expr." << endl;
  expr->code(s);

  if ((index = enviroment->find_variable(name)) != -1)
  {
    if (cgen_comment)
      s << "\t# It is a let variable." << endl;
    emit_store(ACC, index + 1, SP, s);
  }
  else if ((index = enviroment->find_parameter(name)) != -1)
  {
    if (cgen_comment)
      s << "\t# It is a param." << endl;
    emit_store(ACC, index + 3, FP, s);
  }
  else if ((index = enviroment->find_attr(name)) != -1)
  {
    if (cgen_comment)
      s << "\t# It is an attribute." << endl;
    emit_store(ACC, index + 3, SELF, s);
  }
  else
  {
    if (cgen_comment)
      s << "invalid object" << name << endl;
  }
}

void static_dispatch_class::code(ostream &s)
{
}

void dispatch_class::code(ostream &s)
{
  int idx;
  Symbol _class_name;
  CgenNodeP _class_node;

  if (cgen_comment)
    s << "\t# Dispatch " << name << ". First eval and save the params." << endl;

  enviroment->enter_scope();

  for (int i = actual->first(); actual->more(i); i = actual->next(i))
  {
    if (cgen_comment)
      s << "\t# eval parameter " << i << endl;
    actual->nth(i)->code(s);
    if (cgen_comment)
      s << "\t# push parameter " << i << endl;
    emit_push(ACC, s);
    enviroment->fix_variable_index();
  }

  _class_name = enviroment->m_class_node->name;
  _class_node = codegen_classtable->get_class_node(_class_name);
  idx = _class_node->get_dispatch_index_table()[name];

  expr->code(s);

  // if (cgen_comment)
  //   s << "\t# if obj = void: abort" << endl;
  // emit_bne(ACC, ZERO, labelnum, s);
  // s << LA << ACC << " str_const0" << endl;
  // emit_load_imm(T1, 1, s);
  // emit_jal("_dispatch_abort", s);
  // emit_label_def(labelnum, s);
  // ++labelnum;

  if (cgen_comment)
    s << "\t# t1 = self.dispTab" << endl;
  emit_load(T1, 2, ACC, s);
  if (cgen_comment)
    s << "\t# t1 = dispTab[offset]" << endl;
  emit_load(T1, idx, T1, s);
  if (cgen_comment)
    s << "\t# jumpto " << name << endl;
  emit_jalr(T1, s);

  enviroment->exit_scope();
}

void cond_class::code(ostream &s)
{
  if (cgen_comment)
    s << "\t# If statement. First eval condition." << endl;
  pred->code(s);

  if (cgen_comment)
    s << "\t# extract the bool content from acc to t1" << endl;
  emit_fetch_int(T1, ACC, s);
  s << endl;

  int labelnum_false = labelnum++;
  int labelnum_finish = labelnum++;
  // labelnum : false.
  // labelnum + 1: finish
  if (cgen_comment)
    s << "\t# if t1 == 0 goto false" << endl;
  emit_beq(T1, ZERO, labelnum_false, s);
  s << endl;

  then_exp->code(s);

  if (cgen_comment)
    s << "\t# jumpt finish" << endl;
  emit_branch(labelnum_finish, s);
  s << endl;

  if (cgen_comment)
    s << "# False:" << endl;
  emit_label_def(labelnum_false, s);

  else_exp->code(s);

  if (cgen_comment)
    s << "# Finish:" << endl;
  emit_label_def(labelnum_finish, s);
}

void loop_class::code(ostream &s)
{
  int start = labelnum;
  int finish = labelnum + 1;
  labelnum += 2;

  if (cgen_comment)
  {
    s << "\t# While loop" << endl;
    s << "\t# start:" << endl;
  }
  emit_label_def(start, s);
  if (cgen_comment)
    s << "\t# ACC = pred" << endl;
  pred->code(s);
  if (cgen_comment)
    s << "\t# extract int inside bool" << endl;
  emit_fetch_int(T1, ACC, s);
  s << endl;

  if (cgen_comment)
    s << "\t# if pred == false jumpto finish" << endl;
  emit_beq(T1, ZERO, finish, s);
  s << endl;

  body->code(s);

  if (cgen_comment)
    s << "\t# Jumpto start" << endl;
  emit_branch(start, s);

  if (cgen_comment)
    s << "\t# Finish:" << endl;
  emit_label_def(finish, s);

  if (cgen_comment)
    s << "\t# ACC = void" << endl;
  emit_move(ACC, ZERO, s);
}

void typcase_class::code(ostream &s)
{
}

void block_class::code(ostream &s)
{
  if (cgen_comment)
    s << "\t# Block " << endl;
  for (int i = body->first(); body->more(i); i = body->next(i))
  {
    Expression e = body->nth(i);
    if (cgen_comment)
      s << "\t# body " << i << endl;
    e->code(s);
  }
}

void let_class::code(ostream &s)
{
  if (cgen_comment)
  {
    s << "\t# Let expr" << endl;
    s << "\t# First eval init" << endl;
  }
  init->code(s);
  enviroment->enter_scope();
  enviroment->add_variable(identifier);
  if (cgen_comment)
    s << "\t# push ACC" << endl;
  emit_push(ACC, s);
  if (cgen_comment)
    s << "\t# Then eval body" << endl;
  body->code(s);
  if (cgen_comment)
    s << "\t# pop" << endl;
  emit_addiu(SP, SP, 4, s);
  enviroment->exit_scope();
}

void plus_class::code(ostream &s)
{
  if (cgen_comment)
  {
    s << "\t# Int operation : Add" << endl;
    s << "\t# First eval e1 and push." << endl;
  }
  e1->code(s);
  emit_push(ACC, s);
  enviroment->fix_variable_index();

  if (cgen_comment)
    s << "\t# Then eval e2 and make a copy for result." << endl;
  e2->code(s);
  emit_jal("Object.copy", s);

  if (cgen_comment)
    s << "\t# Let's pop e1 to t1, move e2 to t2" << endl;
  emit_addiu(SP, SP, 4, s);
  emit_load(T1, 0, SP, s);
  emit_move(T2, ACC, s);

  if (cgen_comment)
    s << "\t# Extract the int inside the object." << endl;
  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, T2, s);

  if (cgen_comment)
    s << "\t# Modify the int inside t2." << endl;
  emit_add(T3, T1, T2, s);
  emit_store(T3, 3, ACC, s);
}

void sub_class::code(ostream &s)
{
  if (cgen_comment)
  {
    s << "\t# Int operation : Sub" << endl;
    s << "\t# First eval e1 and push." << endl;
  }
  e1->code(s);
  emit_push(ACC, s);
  enviroment->fix_variable_index();

  if (cgen_comment)
    s << "\t# Then eval e2 and make a copy for result." << endl;
  e2->code(s);
  emit_jal("Object.copy", s);

  if (cgen_comment)
    s << "\t# Let's pop e1 to t1, move e2 to t2" << endl;
  emit_addiu(SP, SP, 4, s);
  emit_load(T1, 0, SP, s);
  emit_move(T2, ACC, s);

  if (cgen_comment)
    s << "\t# Extract the int inside the object." << endl;
  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, T2, s);

  if (cgen_comment)
    s << "\t# Modify the int inside t2." << endl;
  emit_sub(T3, T1, T2, s);
  emit_store(T3, 3, ACC, s);
}

void mul_class::code(ostream &s)
{
  if (cgen_comment)
  {
    s << "\t# Int operation : Mul" << endl;
    s << "\t# First eval e1 and push." << endl;
  }
  e1->code(s);
  emit_push(ACC, s);
  enviroment->fix_variable_index();

  if (cgen_comment)
    s << "\t# Then eval e2 and make a copy for result." << endl;
  e2->code(s);
  emit_jal("Object.copy", s);

  if (cgen_comment)
    s << "\t# Let's pop e1 to t1, move e2 to t2" << endl;
  emit_addiu(SP, SP, 4, s);
  emit_load(T1, 0, SP, s);
  emit_move(T2, ACC, s);

  if (cgen_comment)
    s << "\t# Extract the int inside the object." << endl;
  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, T2, s);

  if (cgen_comment)
    s << "\t# Modify the int inside t2." << endl;
  emit_mul(T3, T1, T2, s);
  emit_store(T3, 3, ACC, s);
}

void divide_class::code(ostream &s)
{
  if (cgen_comment)
  {
    s << "\t# Int operation : Div" << endl;
    s << "\t# First eval e1 and push." << endl;
  }
  e1->code(s);
  emit_push(ACC, s);
  enviroment->fix_variable_index();

  if (cgen_comment)
    s << "\t# Then eval e2 and make a copy for result." << endl;
  e2->code(s);
  emit_jal("Object.copy", s);
  s << endl;

  if (cgen_comment)
    s << "\t# Let's pop e1 to t1, move e2 to t2" << endl;
  emit_addiu(SP, SP, 4, s);
  emit_load(T1, 0, SP, s);
  emit_move(T2, ACC, s);
  s << endl;

  if (cgen_comment)
    s << "\t# Extract the int inside the object." << endl;
  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, T2, s);
  s << endl;

  if (cgen_comment)
    s << "\t# Modify the int inside t2." << endl;
  emit_div(T3, T1, T2, s);
  emit_store(T3, 3, ACC, s);
}

void neg_class::code(ostream &s)
{
  if (cgen_comment)
  {
    s << "\t# Neg" << endl;
    s << "\t# Eval e1 and make a copy for result" << endl;
  }
  e1->code(s);
  emit_jal("Object.copy", s);
  s << endl;

  emit_load(T1, 3, ACC, s);
  emit_neg(T1, T1, s);
  emit_store(T1, 3, ACC, s);
}

void lt_class::code(ostream &s)
{
  if (cgen_comment)
  {
    s << "\t# Int operation : Less than" << endl;
    s << "\t# First eval e1 and push." << endl;
  }
  e1->code(s);
  emit_push(ACC, s);
  enviroment->fix_variable_index();

  if (cgen_comment)
    s << "\t# Then eval e2." << endl;
  e2->code(s);
  s << endl;

  if (cgen_comment)
    s << "\t# Let's pop e1 to t1, move e2 to t2" << endl;
  emit_addiu(SP, SP, 4, s);
  emit_load(T1, 0, SP, s);
  emit_move(T2, ACC, s);
  s << endl;

  if (cgen_comment)
    s << "\t# Extract the int inside the object." << endl;
  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, T2, s);
  s << endl;

  if (cgen_comment)
    s << "\t# Pretend that t1 < t2" << endl;
  emit_load_bool(ACC, BoolConst(1), s);
  if (cgen_comment)
    s << "\t# If t1 < t2 jumpto finish" << endl;
  emit_blt(T1, T2, labelnum, s);

  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(labelnum, s);

  ++labelnum;
}

void eq_class::code(ostream &s)
{
  if (cgen_comment)
  {
    s << "\t# equal" << endl;
    s << "\t# First eval e1 and push." << endl;
  }
  e1->code(s);
  emit_push(ACC, s);
  enviroment->fix_variable_index();

  if (cgen_comment)
    s << "\t# Then eval e2." << endl;
  e2->code(s);
  s << endl;

  if (cgen_comment)
    s << "\t# Let's pop e1 to t1, move e2 to t2" << endl;
  emit_addiu(SP, SP, 4, s);
  emit_load(T1, 0, SP, s);
  emit_move(T2, ACC, s);
  s << endl;

  if (e1->type == Int || e1->type == Str || e1->type == Bool)
    if (e2->type == Int || e2->type == Str || e2->type == Bool)
    {
      emit_load_bool(ACC, BoolConst(1), s);
      emit_load_bool(A1, BoolConst(0), s);
      emit_jal("equality_test", s);
      return;
    }

  if (cgen_comment)
    s << "\t# Pretend that t1 = t2" << endl;
  emit_load_bool(ACC, BoolConst(1), s);
  if (cgen_comment)
    s << "\t# Compare the two pointers." << endl;
  emit_beq(T1, T2, labelnum, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(labelnum, s);
  ++labelnum;
}

void leq_class::code(ostream &s)
{
  if (cgen_comment)
  {
    s << "\t# Int operation : Less or equal" << endl;
    s << "\t# First eval e1 and push." << endl;
  }
  e1->code(s);
  emit_push(ACC, s);
  enviroment->fix_variable_index();

  if (cgen_comment)
    s << "\t# Then eval e2." << endl;
  e2->code(s);
  s << endl;

  if (cgen_comment)
    s << "\t# Let's pop e1 to t1, move e2 to t2" << endl;
  emit_addiu(SP, SP, 4, s);
  emit_load(T1, 0, SP, s);
  emit_move(T2, ACC, s);
  s << endl;

  if (cgen_comment)
    s << "\t# Extract the int inside the object." << endl;
  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, T2, s);
  s << endl;

  if (cgen_comment)
    s << "\t# Pretend that t1 < t2" << endl;
  emit_load_bool(ACC, BoolConst(1), s);
  if (cgen_comment)
    s << "\t# If t1 < t2 jumpto finish" << endl;
  emit_bleq(T1, T2, labelnum, s);

  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(labelnum, s);

  ++labelnum;
}

void comp_class::code(ostream &s)
{
  if (cgen_comment)
  {
    s << "\t# the 'not' operator" << endl;
    s << "\t# First eval the bool" << endl;
  }
  e1->code(s);

  if (cgen_comment)
    s << "\t# Extract the int inside the bool" << endl;
  emit_load(T1, 3, ACC, s);

  if (cgen_comment)
    s << "\t# Pretend ACC = false, then we need to construct true" << endl;
  emit_load_bool(ACC, BoolConst(1), s);

  if (cgen_comment)
    s << "\t# If ACC = false, jumpto finish" << endl;
  emit_beq(T1, ZERO, labelnum, s);

  if (cgen_comment)
    s << "\t# Load false" << endl;
  emit_load_bool(ACC, BoolConst(0), s);

  if (cgen_comment)
    s << "\t# finish:" << endl;
  emit_label_def(labelnum, s);

  ++labelnum;
}

void int_const_class::code(ostream &s)
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  IntEntry *entry = inttable.lookup_string(token->get_string());
  if (cgen_comment)
  {
    s << "\t# load int_const: ";
    entry->code_ref(s);
    s << " value " << token->get_string() << endl;
  }
  emit_load_int(ACC, entry, s);
}

void string_const_class::code(ostream &s)
{
  StringEntry *entry = stringtable.lookup_string(token->get_string());
  if (cgen_comment)
  {
    s << "\t# load string_const: ";
    entry->code_ref(s);
    s << " value " << token->get_string() << endl;
  }
  emit_load_string(ACC, entry, s);
}

void bool_const_class::code(ostream &s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s)
{
}

void isvoid_class::code(ostream &s)
{
}

void no_expr_class::code(ostream &s)
{
  emit_move(ACC, ZERO, s);
}

void object_class::code(ostream &s)
{
  int index;
  if (cgen_comment)
    s << "\t# Object " << name << endl;
  if ((index = enviroment->find_variable(name)) != -1)
  {
    if (cgen_comment)
      s << "\t# It is a let variable." << endl;
    emit_load(ACC, index + 1, SP, s);
  }
  else if ((index = enviroment->find_parameter(name)) != -1)
  {
    if (cgen_comment)
      s << "\t# It is a param." << endl;
    emit_load(ACC, index + 3, FP, s);
  }
  else if ((index = enviroment->find_attr(name)) != -1)
  {
    if (cgen_comment)
      s << "\t# It is an attribute." << endl;
    emit_load(ACC, index + 3, SELF, s);
  }
  else if (name == self)
  {
    if (cgen_comment)
      s << "\t# It is self." << endl;
    emit_move(ACC, SELF, s);
  }
  else
  {
    if (cgen_comment)
      s << "invalid object" << name << endl;
  }
}
