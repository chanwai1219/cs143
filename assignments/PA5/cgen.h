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
   std::vector<CgenNodeP> class_nodes;
   std::map<Symbol, int> class_tags;

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
   std::vector<CgenNodeP> get_class_nodes();
   std::map<Symbol, int> get_class_tags();

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
   CgenNode *get_class_node(Symbol class_name);
   int get_class_tag(Symbol s);
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   int class_tag;
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
   void set_class_tag(int tag) { class_tag = tag; }

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

class Environment
{
public:
   Environment(CgenNodeP node) : m_class_node(node) {}

   void enter_scope()
   {
      m_scope_lengths.push_back(0);
   }

   void exit_scope()
   {
      for (int i = 0; i < m_scope_lengths[m_scope_lengths.size() - 1]; ++i)
      {
         m_var_idx_tab.pop_back();
      }
      m_scope_lengths.pop_back();
   }

   int find_attr(Symbol sym)
   {
      std::map<Symbol, int> attr_index_table = m_class_node->get_attr_index_table();
      if (attr_index_table.find(sym) != attr_index_table.end())
      {
         return attr_index_table[sym];
      }
      return -1;
   }

   // The vars are in reverse order.
   int find_variable(Symbol sym)
   {
      for (int idx = m_var_idx_tab.size() - 1; idx >= 0; --idx)
      {
         if (m_var_idx_tab[idx] == sym)
         {
            return m_var_idx_tab.size() - 1 - idx;
         }
      }
      return -1;
   }

   int add_variable(Symbol sym)
   {
      m_var_idx_tab.push_back(sym);
      ++m_scope_lengths[m_scope_lengths.size() - 1];
      return m_var_idx_tab.size() - 1;
   }

   int fix_variable_index()
   {
      extern Symbol No_class;
      return add_variable(No_class);
   }

   int find_parameter(Symbol sym)
   {
      for (unsigned int idx = 0; idx < m_param_idx_tab.size(); ++idx)
      {
         if (m_param_idx_tab[idx] == sym)
         {
            return m_param_idx_tab.size() - 1 - idx;
         }
      }
      return -1;
   }

   int add_parameter(Symbol sym)
   {
      m_param_idx_tab.push_back(sym);
      return m_param_idx_tab.size() - 1;
   }

   std::vector<int> m_scope_lengths;
   std::vector<Symbol> m_var_idx_tab;
   std::vector<Symbol> m_param_idx_tab;
   CgenNodeP m_class_node;
};