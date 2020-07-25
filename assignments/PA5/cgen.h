#include <assert.h>
#include <stdio.h>
#include <vector>
#include <map>
#include <algorithm>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();
   void code_objects();
   void code_object_initializer();
   void code_object_method();

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
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise

   std::vector<method_class *> methods;
   std::vector<method_class *> all_methods;
   std::vector<attr_class *> attrs;
   std::vector<attr_class *> all_attrs;
   std::vector<CgenNode*> inheritance;
   std::map<Symbol, Symbol> dispatch_class_table;
   std::map<Symbol, int> dispatch_index_table;
   std::map<Symbol, int> attr_index_table;

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }

   std::vector<CgenNode*> get_inheritance();

   std::vector<method_class*> get_methods();
   std::vector<method_class*> get_all_methods();
   std::vector<attr_class*> get_attrs();
   std::vector<attr_class*> get_all_attrs();
   std::map<Symbol, Symbol> get_dispatch_class_table() { return dispatch_class_table; }
   std::map<Symbol, int> get_dispatch_index_table() { return dispatch_index_table; }
   std::map<Symbol, int> get_attr_index_table() { return attr_index_table; }
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

