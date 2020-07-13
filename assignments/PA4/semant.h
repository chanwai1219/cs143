#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <algorithm>
#include <map>
#include <list>
#include <set>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;
typedef std::list<Feature> FeatureList;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;
  std::map<Symbol, Class_> m_classes;
  std::map<Symbol, FeatureList*> m_features;
  Classes classes;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
  Class_ get_class(Symbol s);
  Feature get_feature(Symbol class_name, Symbol feature_name);
  void walk_feature(Symbol class_name, void (*cb)(Feature));
  Boolean is_base_of(Symbol base, Symbol derived);
  void semant();
};

#endif

