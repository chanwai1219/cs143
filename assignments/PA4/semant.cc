

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

ClassTable *g_class_table;
SymbolTable<Symbol, Symbol> *g_symbol_table;
Class_ current_class;
method_class *current_method;

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

int is_basic_type(Symbol type)
{
    if (type == Int || type == Str || type == Bool)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

int is_base_of(Symbol base, Symbol derived)
{
    if (derived == NULL)
        return 0;

    while (true)
    {
        if (base == derived)
        {
            return 1;
        }
        if (derived == Object)
        {
            break;
        }
        derived = g_class_table->get_class(derived)->get_parent();
    }

    return 0;
}

Symbol join_type(Symbol s1, Symbol s2)
{
    Class_ class_;
    Symbol type = s1;

    while (type != Object)
    {
        if (is_base_of(type, s1) && is_base_of(type, s2))
        {
            break;
        }

        class_ = g_class_table->get_class(type);
        type = class_->get_parent();
    }

    return type;
}

void feature_walk_cb(Feature f)
{
    if (f->is_method() == 0)
    {
        attr_class *a = (attr_class *)f;
        Symbol name = a->get_name();
        Symbol type = a->get_type();
        g_symbol_table->addid(name, type);
    }
    else
    {
        method_class *m = (method_class *)f;
        Symbol name = m->get_name();
        Symbol type = m->get_type();
        g_symbol_table->addid(name, type);
    }
}

void class__class::semant()
{
    std::list<Symbol> parents;
    Class_ parent_class;
    Symbol parent_name = this->get_name();
    Symbol class_name = this->get_name();
    Features features;
    Feature feature;

    if (g_symbol_table)
    {
        delete g_symbol_table;
    }
    g_symbol_table = new SymbolTable<Symbol, Symbol>();

    if (this->get_parent() == SELF_TYPE)
    {
        g_class_table->semant_error(this) << "Class " << class_name
                                          << " cannot inherit class SELF_TYPE." << std::endl;
        return;
    }

    if (is_basic_type(this->get_parent()))
    {
        g_class_table->semant_error(this) << "Class " << class_name
                                          << " cannot inherit class Bool." << std::endl;
        return;
    }

    while (true)
    {
        parent_class = g_class_table->get_class(parent_name);
        if (parent_class == NULL)
        {
            g_class_table->semant_error(this) << "Class " << class_name
                                              << " inherits from an undefined class " << parent_name << "." << std::endl;
            return;
        }

        parents.push_front(parent_name);

        parent_name = parent_class->get_parent();
        auto it = std::find(parents.begin(), parents.end(), parent_name);
        if (it != parents.end())
        {
            g_class_table->semant_error(this) << "Class " << class_name << ", or an ancestor of "
                                              << class_name << ", is involved in an inheritance cycle." << std::endl;
            return;
        }

        if (parent_name == No_class)
        {
            break;
        }
    }

    current_class = this;
    
    for (std::list<Symbol>::iterator it = parents.begin(); it != parents.end(); ++it)
    {
        parent_class = g_class_table->get_class(*it);
        features = parent_class->get_features();
        for (int i = features->first(); features->more(i); i = features->next(i))
        {
            g_symbol_table->enterscope();
            feature = features->nth(i);
            // if (!feature->is_method())
            {
                Symbol attr_type = g_symbol_table->lookup(feature->get_name());
                if (attr_type != NULL && attr_type != feature->get_type())
                {
                    g_class_table->semant_error(current_class->get_filename(), this)
                        << "Attribute " << this->name << " is an attribute of an inherited class." << std::endl;
                    return;
                }
                g_symbol_table->addid(feature->get_name(), feature->get_type());
            }
            // g_symbol_table->exitscope();
        }
    }

    g_symbol_table->enterscope();

    features = this->get_features();
    g_class_table->walk_feature(this->get_name(), feature_walk_cb);

    for (int i = 0; i < features->len(); i++)
    {
        feature = features->nth(i);

        g_symbol_table->enterscope();
        feature->semant();
        g_symbol_table->exitscope();
    }
    g_symbol_table->exitscope();
}

void method_class::semant()
{
    int i;
    Class_ class_;
    method_class *method, *parent_method;
    Formals formals, parent_formals;
    Formal formal, parent_formal;
    std::set<Symbol> formal_set;
    Symbol parent = current_class->get_parent();
    Symbol expr_type, method_type;

    parent_formals = NULL;

    while (true)
    {
        parent_method = (method_class *)g_class_table->get_feature(parent, name);
        if (parent_method != NULL)
        {
            parent_formals = parent_method->get_formals();
            break;
        }
        class_ = g_class_table->get_class(parent);
        if (class_ == NULL )
        {
            break;
        }
        parent = class_->get_parent();
    }

    formals = this->get_formals();
    if (parent_formals)
    {
        if (parent_formals->len() != formals->len())
        {
            g_class_table->semant_error(current_class->get_filename(), this)
                << "Incompatible number of formal parameters in redefined method " << name << "." << std::endl;
            return;
        }

        for (i = formals->first(); formals->more(i); i = formals->next(i))
        {
            formal = formals->nth(i);
            parent_formal = parent_formals->nth(i);

            if (formal->get_type() != parent_formal->get_type())
            {
                g_class_table->semant_error(current_class->get_filename(), this)
                    << "In redefined method " << name << ", parameter type " << formal->get_type()
                    << " is different from original type " << parent_formal->get_type() << "." << std::endl;
                return;
            }
        }
    }

    for (i = formals->first(); formals->more(i); i = formals->next(i))
    {
        formal = formals->nth(i);
        if (formal->get_name() == self)
        {
            g_class_table->semant_error(current_class->get_filename(), this)
                << "'self' cannot be the name of a formal parameter." << std::endl;
            return;
        }
        if (formal->get_type() == SELF_TYPE)
        {
            g_class_table->semant_error(current_class->get_filename(), this)
                << "Formal parameter " << formal->get_name() << " cannot have type SELF_TYPE." << std::endl;
            return;
        }
        if (formal_set.find(formal->get_name()) != formal_set.end())
        {
            g_class_table->semant_error(current_class->get_filename(), this)
                << "Formal parameter " << formal->get_name() << " is multiply defined." << std::endl;
            return;
        }
        formal_set.insert(formal->get_name());
        formal->semant();
    }

    this->expr->semant();

    expr_type = expr->get_type();
    if (expr_type)
    {
        if (expr_type == SELF_TYPE)
        {
            expr_type = current_class->get_name();
        }

        if (return_type == SELF_TYPE)
        {
            method_type = current_class->get_name();
        }
        else
        {
            method_type = return_type;
        }

        if (!is_base_of(method_type, expr_type))
        {
            g_class_table->semant_error(current_class->get_filename(), this)
                << "Inferred return type " << expr_type << " of method " << name
                << " does not conform to declared return type " << return_type << "." << std::endl;
            return;
        }
    }
}

void attr_class::semant()
{
    Symbol ltype, rtype;

    if (this->name == self)
    {
        g_class_table->semant_error(current_class->get_filename(), this)
            << "'self' cannot be the name of an attribute." << std::endl;

        return;
    }
    if (g_class_table->get_class(this->type_decl) == NULL)
    {
        g_class_table->semant_error(current_class->get_filename(), this)
            << "Class " << this->type_decl << " of attribute a is undefined." << std::endl;
        return;
    }

    ltype = g_symbol_table->lookup(this->name);
    rtype = this->init->get_type();
    if (rtype != NULL && !is_base_of(ltype, rtype))
    {
        g_class_table->semant_error(current_class->get_filename(), this)
            << "Class " << this->type_decl << " of attribute a is undefined." << std::endl;
        return;
    }

    this->init->semant();
}

void formal_class::semant()
{
    if (g_class_table->get_class(this->type_decl) == NULL)
    {
        g_class_table->semant_error(current_class->get_filename(), this)
            << "Class " << this->type_decl << " of formal parameter " << this->name << " is undefined." << std::endl;
        return;
    }

    Symbol name = this->get_name();
    Symbol type = this->get_type();
    g_symbol_table->addid(name, type);
}

void branch_class::semant()
{
    g_symbol_table->addid(name, type_decl);
    this->expr->semant();
}

void assign_class::semant()
{
    Symbol ltype, rtype;

    this->expr->semant();
    rtype = this->expr->get_type();
    ltype = g_symbol_table->lookup(name);

    if (this->name == self)
    {
        g_class_table->semant_error(current_class->get_filename(), this)
            << "Cannot assign to 'self'." << std::endl;
        return;
    }

    if (!is_base_of(ltype, rtype))
    {
        g_class_table->semant_error(current_class->get_filename(), this)
            << "Type " << rtype << " of assigned expression does not conform to declared type "
            << ltype << " of identifier " << this->name << "." << std::endl;
        return;
    }

    this->set_type(rtype);
}

void static_dispatch_class::semant()
{
    int i;
    Class_ class_;
    Symbol expr_type;
    method_class *method;
    Symbol actual_type, formal_type;

    this->expr->semant();
    for (int i = actual->first(); actual->more(i); i = actual->next(i))
    {
        actual->nth(i)->semant();
    }

    expr_type = this->expr->get_type();
    if (!is_base_of(type_name, expr_type))
    {
        g_class_table->semant_error(current_class->get_filename(), this)
            << "Expression type " << expr_type
            << " does not conform to declared static dispatch type " << type_name
            << "." << std::endl;
        return;
    }

    while (true)
    {
        class_ = g_class_table->get_class(expr_type);
        if (class_ == NULL)
        {
            g_class_table->semant_error(current_class->get_filename(), this)
                << "Dispatch to undefined method " << get_name() << "." << std::endl;
            return;
        }

        method = (method_class *)g_class_table->get_feature(class_->get_name(), this->get_name());
        if (method != NULL)
        {
            break;
        }

        expr_type = class_->get_parent();
    }

    if (actual->len() != method->get_formals()->len())
    {
        g_class_table->semant_error(current_class->get_filename(), this)
            << "param number mismatch." << std::endl;
        return;
    }

    for (i = actual->first(); actual->more(i); i = actual->next(i))
    {
        actual_type = actual->nth(i)->get_type();
        formal_type = method->get_formals()->nth(i)->get_type();
        if (!is_base_of(formal_type, actual_type))
        {
            g_class_table->semant_error(current_class->get_filename(), this)
                << "In call of method " << this->name
                << ", type " << actual_type << " of parameter " << method->get_formals()->nth(i)->get_name()
                << " does not conform to declared type " << method->get_formals()->nth(i)->get_type()
                << "." << std::endl;
            return;
        }
    }

    this->set_type(method->get_type());
}

void dispatch_class::semant()
{
    int i;
    Symbol expr_type;
    Class_ class_;
    method_class *method;
    Symbol actual_type, formal_type;

    this->expr->semant();
    for (int i = actual->first(); actual->more(i); i = actual->next(i))
    {
        actual->nth(i)->semant();
    }

    expr_type = this->expr->get_type();
    class_ = g_class_table->get_class(expr_type);
    
    while (true)
    {
        class_ = g_class_table->get_class(expr_type);
        if (class_ == NULL)
        {
            g_class_table->semant_error(current_class->get_filename(), this)
                << "Dispatch to undefined method " << get_name() << "." << std::endl;
            return;
        }

        method = (method_class *)g_class_table->get_feature(class_->get_name(), this->get_name());
        if (method != NULL)
        {
            break;
        }

        expr_type = class_->get_parent();
    }

    if (actual->len() != method->get_formals()->len())
    {
        g_class_table->semant_error(current_class->get_filename(), this)
            << "param number mismatch." << std::endl;
        return;
    }

    for (i = actual->first(); actual->more(i); i = actual->next(i))
    {
        actual_type = actual->nth(i)->get_type();
        formal_type = method->get_formals()->nth(i)->get_type();
        if (actual_type == SELF_TYPE)
        {
            actual_type = current_class->get_name();
        }
        if (!is_base_of(formal_type, actual_type))
        {
            g_class_table->semant_error(current_class->get_filename(), this)
                << "In call of method " << this->name
                << ", type " << actual_type << " of parameter " << method->get_formals()->nth(i)->get_name()
                << " does not conform to declared type " << method->get_formals()->nth(i)->get_type()
                << "." << std::endl;
            return;
        }
    }

    if (method->get_type() != SELF_TYPE)
        this->set_type(method->get_type());
    else
        this->set_type(this->expr->get_type());
}

void cond_class::semant()
{
    pred->semant();
    then_exp->semant();
    else_exp->semant();
    this->set_type(join_type(then_exp->get_type(), else_exp->get_type()));
}

void loop_class::semant()
{
    pred->semant();
    if (pred->get_type() != Bool)
    {
        g_class_table->semant_error(current_class->get_filename(), this)
            << "Loop condition does not have type Bool." << std::endl;
        return;
    }
    body->semant();
    this->set_type(Object);
}

void typcase_class::semant()
{
    std::set<Symbol> case_set;
    branch_class *branch;
    Symbol case_type, branch_type;

    expr->semant();

    for (int i = cases->first(); cases->more(i); i = cases->next(i))
    {
        branch = (branch_class *)cases->nth(i);

        g_symbol_table->enterscope();
        branch->semant();
        g_symbol_table->exitscope();

        if (case_set.find(branch->get_type()) != case_set.end())
        {
            g_class_table->semant_error(current_class->get_filename(), this)
                << "Duplicate branch Int in case statement." << std::endl;
            return;
        }

        branch_type = branch->get_expression()->get_type();
        if (i == 0)
        {
            case_type = branch_type;
        }
        else
        {
            case_type = join_type(case_type, branch_type);
        }

        case_set.insert(branch->get_type());
    }

    this->set_type(case_type);
}

void block_class::semant()
{
    int i;
    Expression exp;

    for (i = body->first(); body->more(i); i = body->next(i))
    {
        exp = body->nth(i);
        exp->semant();

        if (i == (body->len() - 1))
        {
            this->set_type(exp->get_type());
        }
    }
}

void let_class::semant()
{
    if (identifier == self)
    {
        g_class_table->semant_error(current_class->get_filename(), this)
            << "'self' cannot be bound in a 'let' expression." << std::endl;
        return;
    }

    g_symbol_table->enterscope();
    g_symbol_table->addid(identifier, type_decl);

    init->semant();
    if (init->get_type() != No_type)
    {
        if (!is_base_of(type_decl, init->get_type()))
        {
            g_class_table->semant_error(current_class->get_filename(), this)
                << "Inferred type " << init->get_type() << " of initialization of " << this->identifier
                << " does not conform to identifier's declared type " << this->type_decl << "." << std::endl;
            return;
        }
    }

    body->semant();
    g_symbol_table->exitscope();

    this->set_type(body->get_type());
}

void plus_class::semant()
{
    Symbol type1, type2;

    e1->semant();
    e2->semant();

    type1 = e1->get_type();
    type2 = e2->get_type();

    if (type1 != Int || type2 != Int)
    {
        g_class_table->semant_error(current_class->get_filename(), this)
            << "non-Int arguments: "<< type1 << " + " << type2 << std::endl;
        return;
    }

    this->set_type(e1->get_type());
}

void sub_class::semant()
{
    Symbol type1, type2;

    e1->semant();
    e2->semant();

    type1 = e1->get_type();
    type2 = e2->get_type();

    if (type1 != Int || type2 != Int)
    {
        g_class_table->semant_error(current_class->get_filename(), this)
            << "non-Int arguments: "<< type1 << " - " << type2 << std::endl;
        return;
    }
    this->set_type(e1->get_type());
}

void mul_class::semant()
{
    Symbol type1, type2;

    e1->semant();
    e2->semant();

    type1 = e1->get_type();
    type2 = e2->get_type();

    if (type1 != Int || type2 != Int)
    {
        g_class_table->semant_error(current_class->get_filename(), this)
            << "non-Int arguments: "<< type1 << " * " << type2 << std::endl;
        return;
    }
    this->set_type(e1->get_type());
}

void divide_class::semant()
{
    Symbol type1, type2;

    e1->semant();
    e2->semant();

    type1 = e1->get_type();
    type2 = e2->get_type();

    if (type1 != Int || type2 != Int)
    {
        g_class_table->semant_error(current_class->get_filename(), this)
            << "non-Int arguments: "<< type1 << " / " << type2 << std::endl;
        return;
    }
    this->set_type(e1->get_type());
}

void neg_class::semant()
{
    e1->semant();
    this->set_type(e1->get_type());
}

void lt_class::semant()
{
    Symbol type1, type2;

    e1->semant();
    e2->semant();

    type1 = e1->get_type();
    type2 = e2->get_type();

    if (type1 != type2)
    {
        g_class_table->semant_error(current_class->get_filename(), this)
            << "type mismatch." << std::endl;
        return;
    }
    this->set_type(Bool);
}

void eq_class::semant()
{
    Symbol type1, type2;

    e1->semant();
    e2->semant();

    type1 = e1->get_type();
    type2 = e2->get_type();

    if (is_basic_type(type1) || is_basic_type(type2))
    {
        if (type1 != type2)
        {
            g_class_table->semant_error(current_class->get_filename(), this)
                << "Illegal comparison with a basic type." << std::endl;
            return;
        }
    }

    this->set_type(Bool);
}

void leq_class::semant()
{
    Symbol type1, type2;

    e1->semant();
    e2->semant();

    type1 = e1->get_type();
    type2 = e2->get_type();

    if (type1 != type2)
    {
        g_class_table->semant_error(current_class->get_filename(), this)
            << "type mismatch." << std::endl;
        return;
    }
    this->set_type(Bool);
}

void comp_class::semant()
{
    e1->semant();
    this->set_type(Bool);
}

void int_const_class::semant()
{
    this->set_type(Int);
}

void bool_const_class::semant()
{
    this->set_type(Bool);
}

void string_const_class::semant()
{
    this->set_type(Str);
}

void new__class::semant()
{
    if (g_class_table->get_class(type_name) == NULL)
    {
        g_class_table->semant_error(current_class->get_filename(), this)
            << "'new' used with undefined class" << type_name << std::endl;
        return;
    }
    this->set_type(type_name);
}

void isvoid_class::semant()
{
    e1->semant();
    this->set_type(Bool);
}

void no_expr_class::semant()
{
    this->set_type(No_type);
}

void object_class::semant()
{
    Symbol type;

    if (name == self)
    {
        type = SELF_TYPE;
        // type = current_class->get_name();
    }
    else
    {
        type = g_symbol_table->lookup(name);
        if (type == NULL)
        {
            g_class_table->semant_error(current_class->get_filename(), this)
                << "Undeclared identifier " << this->name << "." << std::endl;
            return;
        }
    }
    this->set_type(type);
}

void ClassTable::semant()
{
    int i;

    for (i = classes->len() - 1; i >= 0; i--)
    {
        classes->nth(i)->semant();
    }
}

Class_ ClassTable::get_class(Symbol s)
{
    std::map<Symbol, Class_>::iterator iter;

    if (s == SELF_TYPE)
        return current_class;

    iter = m_classes.find(s);
    if (iter == m_classes.end())
        return NULL;
    else
        return iter->second;
}

Feature ClassTable::get_feature(Symbol class_name, Symbol feature_name)
{
    Feature feature;
    FeatureList *feature_list;
    std::map<Symbol, FeatureList*>::iterator map_it;
    std::list<Feature>::iterator list_it;

    map_it = m_features.find(class_name);
    if (map_it == m_features.end())
        return NULL;

    feature_list = map_it->second;

    for (list_it = feature_list->begin(); list_it != feature_list->end(); ++list_it)
    {
        feature = *list_it;
        if (feature->get_name() == feature_name)
        {
            return feature;
        }
    }

    return NULL;
}

void ClassTable::walk_feature(Symbol class_name, void (*cb)(Feature))
{
    FeatureList *feature_list;
    std::map<Symbol, FeatureList*>::iterator map_it;
    std::list<Feature>::iterator list_it;

    map_it = m_features.find(class_name);
    if (map_it == m_features.end())
        return;

    feature_list = map_it->second;

    for (list_it = feature_list->begin(); list_it != feature_list->end(); ++list_it)
    {
        cb(*list_it);
    }
}

ClassTable::ClassTable(Classes classes) :  semant_errors(0) , error_stream(cerr) {
    int i, j, k;
    int main = 0;
    std::map<Symbol, Class_>::iterator class_iter;

    this->classes = classes;
    this->install_basic_classes();

    for (i = classes->first(); classes->more(i); i = classes->next(i))
    {
        
        Symbol class_name = classes->nth(i)->get_name();
        
        if (class_name == Main)
        {
            main = 1;
        }

        if (m_classes.find(class_name) == m_classes.end())
        {
            m_classes.insert(std::make_pair(class_name, classes->nth(i)));
        }
        else
        {
            semant_error(classes->nth(i)) << "Class " << class_name << " was previously defined." << std::endl;
            return;
        }
    }

    if (!main)
    {
        error_stream << "Class Main is not defined." << std::endl;
        semant_error();
        return;
    }

    for (class_iter = m_classes.begin(); class_iter != m_classes.end(); ++class_iter)
    {
        FeatureList *feature_list;
        Features features = class_iter->second->get_features();
        std::map<Symbol, FeatureList *>::iterator iter;

        iter = m_features.find(class_iter->first);
        if (iter == m_features.end())
        {
            feature_list = new FeatureList();
            m_features.insert(std::make_pair(class_iter->first, feature_list));
        }
        else
        {
            feature_list = iter->second;
        }

        for (j = features->first(); features->more(j); j = features->next(j))
        {
            feature_list->push_back(features->nth(j));

            features->nth(j)->set_class(class_iter->second->get_name());

            if (features->nth(j)->is_method() == FALSE)
            {
                Symbol name = features->nth(j)->get_name();
                Symbol type = features->nth(j)->get_type();
            }
        }
    }
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotaj.jte the classes built below.
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
    m_classes.insert(std::make_pair(Object, Object_class));

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
    m_classes.insert(std::make_pair(IO, IO_class));

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);
    m_classes.insert(std::make_pair(Int, Int_class));

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);
    m_classes.insert(std::make_pair(Bool, Bool_class));

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
    m_classes.insert(std::make_pair(Str, Str_class));
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
    g_class_table = classtable;

    g_class_table->semant();

    /* some semantic analysis code may go here */

    if (classtable->errors())
    {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}


